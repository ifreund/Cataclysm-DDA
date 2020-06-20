#include "shadowcasting.h"

#include "fragment_cloud.h"
#include "game.h"
#include "line.h"
#include "point.h"

struct slope {
    slope( int rise, int run ) {
        // Ensure run is always positive for the inequality operators
        this->run = abs( run );
        if( run < 0 ) {
            this->rise = -rise;
        } else {
            this->rise = rise;
        }
    }

    int rise;
    int run;
};

inline bool operator<( const slope &lhs, const slope &rhs )
{
    // a/b < c/d <=> a*d < c*b if b and d have the same sign.
    return lhs.rise * rhs.run < rhs.rise * lhs.run;
}

inline bool operator>( const slope &lhs, const slope &rhs )
{
    return rhs < lhs;
}

inline bool operator<=( const slope &lhs, const slope &rhs )
{
    return !( lhs > rhs );
}

inline bool operator>=( const slope &lhs, const slope &rhs )
{
    return !( lhs < rhs );
}

inline bool operator==( const slope &lhs, const slope &rhs )
{
    // a/b == c/d <=> a*d == c*b
    return lhs.rise * rhs.run == rhs.rise * lhs.run;
}

inline bool operator!=( const slope &lhs, const slope &rhs )
{
    return !( lhs == rhs );
}

template<typename T>
struct span {
    slope start_row;
    slope end_row;
    slope start_col;
    slope end_col;
    T cumulative_value;
};

template<typename T>
struct state {
    T cur_row;
    T next_row;
};

template<int xx, int xy, int xz, int yx, int yy, int yz, int zz, typename T,
         T( *calc )( const T &, const T &, const int & ),
         bool( *is_transparent )( const T &, const T & ),
         T( *accumulate )( const T &, const T &, const int & )>
void cast_zlight_segment(
    const array_of_grids_of<T> &output_caches,
    const array_of_grids_of<const T> &input_arrays,
    const array_of_grids_of<const bool> &floor_caches,
    const tripoint &offset, const int offset_distance,
    const T numerator )
{
    const int radius = 60 - offset_distance;

    T last_intensity = 0;
    tripoint delta{0,0,0};
    tripoint current{0,0,0};

    // We start out with one span covering the entire horizontal and vertical space
    // we are interested in.  Then as changes in transparency are encountered, we truncate
    // that initial span and insert new spans before/after it in the list, removing any that
    // are no longer needed as we go.
    std::list<span<T>> spans = { {
            {1,1}, {0,1},
            {1,1}, {0,1},
            LIGHT_TRANSPARENCY_OPEN_AIR
        }
    };

    // At each "depth", a.k.a. distance from the origin, we iterate once over the list of spans,
    // possibly splitting them.
    for( int distance = 1; distance <= radius && !spans.empty(); ++distance) {
        delta.x = distance;

        for( auto span_it = spans.begin(); span_it != spans.end(); ) {

            bool started_block = false;
            state<T> cur_state{0,0};

            for( delta.z = distance; delta.z >= 0; --delta.z) {
                const slope high_row{delta.z * 2 + 1, delta.x * 2 - 1};
                const slope low_row{delta.z * 2 - 1, delta.x * 2 + 1};

                current.z = offset.z + delta.z * zz;
                if( current.z > OVERMAP_HEIGHT || current.z < -OVERMAP_DEPTH || // current point out of bounds
                    span_it->end_row > high_row ) { // The span doesn't touch this row, skip to the next row
                    continue;
                } else if( span_it->start_row < low_row) {
                    // We are done with the current span, skip to the next span
                    break;
                }

                cata::optional<int> next_z = current.z + zz;
                {
                    const slope next_high_row{(delta.z - 1) * 2 + 1, delta.x * 2 - 1};
                    const slope next_low_row{(delta.z - 1) * 2 - 1, delta.x * 2 + 1};

                    if( *next_z > OVERMAP_HEIGHT || *next_z < -OVERMAP_DEPTH || // current point out of bounds
                        span_it->end_row > next_high_row || // The span doesn't touch this row, skip to the next row
                        span_it->start_row < next_low_row) {
                        next_z = cata::nullopt;
                    }
                }

                for( delta.y = distance; delta.y >= 0; --delta.y ) {
                    current.x = offset.x + delta.x * xx + delta.y * xy + delta.z * xz;
                    current.y = offset.y + delta.x * yx + delta.y * yy + delta.z * yz;

                    const slope high_col{delta.y * 2 + 1, delta.x * 2 - 1};
                    const slope low_col{delta.y * 2 - 1, delta.x * 2 + 1};

                    if( current.x < 0 || current.x >= MAPSIZE_X ||
                        current.y < 0 || current.y >= MAPSIZE_Y || // Current point is out of bounds, advance to the next column.
                        span_it->end_col > high_col ){ // Current point comes before the span we're considering, advance to the next column.
                        continue;
                    } else if( span_it->start_col < low_col ) {
                        // Current point is after the span we're considering, continue to next row.
                        break;
                    }

                    auto get_transparency = [&offset, &floor_caches, &input_arrays](const tripoint &abs_point, bool &floor_block) -> T {
                        // If we're looking at a point with floor or roof from the floor/roof side,
                        //  that tile is actually invisible to us.
                        const int z_index = abs_point.z + OVERMAP_DEPTH;
                        if( ( abs_point.z < offset.z && z_index < OVERMAP_LAYERS - 1 && ( *floor_caches[z_index + 1] )[abs_point.x][abs_point.y]) ||
                            ( abs_point.z > offset.z && ( *floor_caches[z_index] )[abs_point.x][abs_point.y] ) ) {
                            floor_block = true;
                            return LIGHT_TRANSPARENCY_SOLID;
                        } else {
                            floor_block = false;
                            return ( *input_arrays[z_index] )[abs_point.x][abs_point.y];
                        }
                    };

                    bool floor_block;
                    bool dummy; // never used (this is bad code)
                    state<T> new_state;
                    new_state.cur_row = get_transparency( current, floor_block );
                    new_state.next_row = next_z ? get_transparency( {current.x, current.y, *next_z}, dummy ) : cur_state.cur_row;

                    if( !started_block ) {
                        started_block = true;
                        cur_state = new_state;
                    }

                    const int dist = rl_dist( tripoint_zero, delta ) + offset_distance;
                    last_intensity = calc( numerator, span_it->cumulative_value, dist );

                    if( !floor_block ) {
                        const int z_index = current.z + OVERMAP_DEPTH;
                        ( *output_caches[z_index] )[current.x][current.y] =
                            std::max( ( *output_caches[z_index] )[current.x][current.y], last_intensity );
                    }

                    if(cur_state.cur_row == new_state.cur_row && cur_state.next_row == new_state.next_row) {
                        // No change, no need to split the span
                        continue;
                    }

                    // sweep direction -->
                    // 
                    // +---+---+ <- start row
                    // | A | B |
                    // |   +---+ <- 
                    // |   | C |
                    // |   +---+ <- trailing row
                    // |   | D |
                    // | ? | ? |
                    // | ? | ? |
                    // +---+---+ <- end row
                    // ^       ^
                    // |       end col
                    // start col
                    // A is previously processed row(s).
                    // B is already-processed tiles from current row.

                    const T next_cumulative_transparency = accumulate( span_it->cumulative_value, cur_state.cur_row, distance );

                    const slope next_high_row{(delta.z - 1) * 2 + 1, delta.x * 2 - 1};

                    if (is_transparent(cur_state.cur_row, last_intensity)) {
                        // Insert before
                        spans.insert(span_it, {span_it->start_row, next_high_row,
                                               span_it->start_col, high_col,
                                               next_cumulative_transparency});
                    }

                    if (is_transparent(cur_state.next_row, last_intensity)) {
                        // Insert after
                        spans.insert(std::next(span_it), {next_high_row, span_it->end_row,
                                                          span_it->start_col, high_col,
                                                          span_it->cumulative_value});
                    }

                    span_it->start_col = high_col;
                    cur_state = new_state;
                }

                // Ended a row with differing transparencies, need to split
                if (cur_state.cur_row != cur_state.next_row) {
                    const slope next_high_row{(delta.z - 1) * 2 + 1, delta.x * 2 - 1};
                    if (is_transparent(cur_state.cur_row, last_intensity)) {
                        const T next_cumulative_transparency = accumulate( span_it->cumulative_value, cur_state.cur_row, distance );
                        // Insert before
                        spans.insert(span_it, {span_it->start_row, next_high_row,
                                               span_it->start_col, span_it->end_row,
                                               next_cumulative_transparency});
                    }
                    span_it->start_row = next_high_row;
                }
            }

            if (!started_block || // Nothing was scanned, must be out of bounds
                !is_transparent(cur_state.cur_row, last_intensity)) { // Span is opaque, no need to iterate it further
                // Remove the span from the list
                span_it = spans.erase( span_it );
            } else {
                span_it->cumulative_value = accumulate( span_it->cumulative_value, cur_state.cur_row, distance );
                ++span_it;
            }
        }
    }
}

template<typename T, T( *calc )( const T &, const T &, const int & ),
         bool( *check )( const T &, const T & ),
         T( *accumulate )( const T &, const T &, const int & )>
void cast_zlight(
    const array_of_grids_of<T> &output_caches,
    const array_of_grids_of<const T> &input_arrays,
    const array_of_grids_of<const bool> &floor_caches,
    const tripoint &origin, const int offset_distance, const T numerator )
{
    // Down
    cast_zlight_segment < 0, 1, 0, 1, 0, 0, -1, T, calc, check, accumulate > (
        output_caches, input_arrays, floor_caches, origin, offset_distance, numerator );
    cast_zlight_segment < 1, 0, 0, 0, 1, 0, -1, T, calc, check, accumulate > (
        output_caches, input_arrays, floor_caches, origin, offset_distance, numerator );

    cast_zlight_segment < 0, -1, 0, 1, 0, 0, -1, T, calc, check, accumulate > (
        output_caches, input_arrays, floor_caches, origin, offset_distance, numerator );
    cast_zlight_segment < -1, 0, 0, 0, 1, 0, -1, T, calc, check, accumulate > (
        output_caches, input_arrays, floor_caches, origin, offset_distance, numerator );

    cast_zlight_segment < 0, 1, 0, -1, 0, 0, -1, T, calc, check, accumulate > (
        output_caches, input_arrays, floor_caches, origin, offset_distance, numerator );
    cast_zlight_segment < 1, 0, 0, 0, -1, 0, -1, T, calc, check, accumulate > (
        output_caches, input_arrays, floor_caches, origin, offset_distance, numerator );

    cast_zlight_segment < 0, -1, 0, -1, 0, 0, -1, T, calc, check, accumulate > (
        output_caches, input_arrays, floor_caches, origin, offset_distance, numerator );
    cast_zlight_segment < -1, 0, 0, 0, -1, 0, -1, T, calc, check, accumulate > (
        output_caches, input_arrays, floor_caches, origin, offset_distance, numerator );

    // Up
    cast_zlight_segment<0, 1, 0, 1, 0, 0, 1, T, calc, check, accumulate>(
        output_caches, input_arrays, floor_caches, origin, offset_distance, numerator );
    cast_zlight_segment<1, 0, 0, 0, 1, 0, 1, T, calc, check, accumulate>(
        output_caches, input_arrays, floor_caches, origin, offset_distance, numerator );

    cast_zlight_segment < 0, -1, 0, 1, 0, 0, 1, T, calc, check, accumulate > (
        output_caches, input_arrays, floor_caches, origin, offset_distance, numerator );
    cast_zlight_segment < -1, 0, 0, 0, 1, 0, 1, T, calc, check, accumulate > (
        output_caches, input_arrays, floor_caches, origin, offset_distance, numerator );

    cast_zlight_segment < 0, 1, 0, -1, 0, 0, 1, T, calc, check, accumulate > (
        output_caches, input_arrays, floor_caches, origin, offset_distance, numerator );
    cast_zlight_segment < 1, 0, 0, 0, -1, 0, 1, T, calc, check, accumulate > (
        output_caches, input_arrays, floor_caches, origin, offset_distance, numerator );

    cast_zlight_segment < 0, -1, 0, -1, 0, 0, 1, T, calc, check, accumulate > (
        output_caches, input_arrays, floor_caches, origin, offset_distance, numerator );
    cast_zlight_segment < -1, 0, 0, 0, -1, 0, 1, T, calc, check, accumulate > (
        output_caches, input_arrays, floor_caches, origin, offset_distance, numerator );
}

// I can't figure out how to make implicit instantiation work when the parameters of
// the template-supplied function pointers are involved, so I'm explicitly instantiating instead.
template void cast_zlight<float, sight_calc, sight_check, accumulate_transparency>(
    const array_of_grids_of<float> &output_caches,
    const array_of_grids_of<const float> &input_arrays,
    const array_of_grids_of<const bool> &floor_caches,
    const tripoint &origin, int offset_distance, float numerator );

template void cast_zlight<fragment_cloud, shrapnel_calc, shrapnel_check, accumulate_fragment_cloud>(
    const array_of_grids_of<fragment_cloud> &output_caches,
    const array_of_grids_of<const fragment_cloud> &input_arrays,
    const array_of_grids_of<const bool> &floor_caches,
    const tripoint &origin, int offset_distance, fragment_cloud numerator );

