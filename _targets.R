
library(targets)
library(tarchetypes)

source('packages.R')
lapply(list.files("./R", full.names = TRUE), source)

tar_plan(

         target_postcode_sf =

           absmapsdata::postcode2016 %>%
           mutate( total_postcode_area = as.numeric( st_area( geometry )))

         ,

         ####
         # Remoteness #
         ####

         postcode_intersect_remote =

           st_intersection( target_postcode_sf, absmapsdata::ra2016 )  %>%
           mutate(postcode_intersect_remote_area = as.numeric( st_area(.))) %>%
           dplyr::select( postcode_2016, ra_code, ra, postcode_intersect_remote_area) %>%
           st_drop_geometry()

         ,

         postcode_remote_area_pct =

           target_postcode_sf %>%
           st_drop_geometry () %>%
           inner_join( postcode_intersect_remote, by =c( 'postcode_2016')) %>%
           mutate( pct = round( as.numeric( postcode_intersect_remote_area / total_postcode_area) * 100, 2 ))

         ,

         postcode_remote_pct_wide =

           postcode_remote_area_pct %>%
           select( postcode_2016, ra, pct ) %>%
           count( postcode_2016, ra , wt=pct, name='pct') %>%
           pivot_wider( postcode_2016, names_from=ra, values_from=pct, values_fill=0) ,

         ,

         # just take the largest remoteness for this postcode
         postcode_best_remote =

           target_postcode_sf %>%
           st_join( ra2016, largest=TRUE ) %>%
           st_drop_geometry() %>%
           select( postcode_2016, ra_code, ra ) %>%
           mutate( ra_digit2 = str_sub( as.character( ra_code), 2, 2 ))

         ,

         #######################################################################
         #                                   sa1                               #
         #######################################################################

         sa1_population =

           read_xls( 'data/2033055001_sa1_indexes_seifa.xls', sheet='Table 2', skip=5 ) %>%
           janitor::clean_names() %>%
           mutate( sa1_7dig_2016 = as.character( x1 ) ) %>%
           rename( population = x3 ) %>%
           select(  sa1_7dig_2016,  population)

         ,

         seifa_sa1 =

           read_xls('data/2033055001_sa1_indexes_seifa.xls', skip=5, sheet='Table 1') %>%
           clean_names() %>%
           select( -x11 ) %>%
           set_names( wrapr::qc( sa1_7dig_2016, sa1_maincode_2016,
                                irsd_score, irsd_decile,
                                irsad_score, irsad_decile,
                                economic_score, economic_decile,
                                education_score, education_decile)) %>%
           mutate( across( starts_with( 'sa1'), as.character ))%>%
           mutate( across( c( ends_with('decile'), ends_with( 'score')), as.numeric )) %>%
           select(  sa1_7dig_2016,  ends_with('decile'), ends_with("score") ) %>%
           pivot_longer( cols=c( ends_with('decile'), ends_with("score")), names_to='measure', values_to='score_decile') %>%
           drop_na( score_decile )

         ,

         seifa_sa2 =

           read_xls('data/2033055001_sa2_indexes_seifa.xls', skip=5, sheet='Table 1') %>%
           clean_names() %>%
           select( -x11 ) %>%
           set_names( wrapr::qc( sa2_maincode_2016, sa2_name_2016,
                                irsd_score, irsd_decile,
                                irsad_score, irsad_decile,
                                economic_score, economic_decile,
                                education_score, education_decile)) %>%
           mutate( across( starts_with( 'sa2'), as.character )) %>%
           mutate( across( c( ends_with('decile'), ends_with( 'score')), as.numeric )) %>%
           select(  sa2_maincode_2016,  ends_with('decile'), ends_with("score") ) %>%
           pivot_longer( cols=c( ends_with('decile'), ends_with("score")), names_to='measure', values_to='score_decile') %>%
           drop_na( score_decile )

         ,

         sa1_to_sa2 =

           read_csv('data/SA1_2016_AUST_sa1_to_sa2_correspondence.csv' ) %>%
           clean_names() %>%
           rename( sa1_7dig_2016 = sa1_7digitcode_2016 ) %>%
           mutate( across( ends_with('2016'), as.character))

         ,

         # full list of all possible seifa deciles.  This is the one we are trying to match
         all_deciles =

           expand_grid(
                       measure=c("irsd_decile", "irsad_decile", "economic_decile", "education_decile"),
                       distinct( sa1_to_sa2, sa1_7dig_2016,  sa2_maincode_2016)
           )

         ,

         # take all the deciles from sa2 that are not in sa1, add back in sa1
         best_sa1_decile =

           all_deciles %>%
           anti_join( seifa_sa1 , by = c("measure", "sa1_7dig_2016") ) %>%
           inner_join( seifa_sa2, by = c("measure", "sa2_maincode_2016") ) %>%
           bind_rows( seifa_sa1 ) %>%
           select( -sa2_maincode_2016 )
         ,

         best_sa1_decile_wide =

           best_sa1_decile %>%
           pivot_wider( names_from=measure, values_from=score_decile )

         ,

         # add in geometry to decile
         sa1_all =

           sa12016 %>%
           mutate( sa1_area = as.numeric( st_area( geometry ))) %>%
           mutate( geom_sa1 = st_geometry( geometry )) %>%
           inner_join( best_sa1_decile_wide, by='sa1_7dig_2016')

         ,

         # join to postcode, calculate overlap
         postcode_sa1 =

           target_postcode_sf %>%
           st_join( sa1_all ) %>%
           mutate( postcode_intersect_sa1_area =

                  purrr::map2_dbl( geometry,
                                  geom_sa1,
                                  ~as.numeric( sf::st_area( sf::st_intersection( .x, .y ))) * 10E9)) %>%
           filter( postcode_intersect_sa1_area > 0 ) %>%
           st_drop_geometry () %>%
           select(-geom_sa1 ) %>%
           as_tibble()

         ,

         # calculate areas intersection, use that to find the population weighting
         # assuming the population is spread evenly throughout the sa1, take the
         # intersection area proportion of the population
         # and allot it as portion of the postcode
         postcode_sa1_weighted =

           postcode_sa1 %>%
           left_join( sa1_population , by = 'sa1_7dig_2016' ) %>%
           replace_na( list( population=0)) %>%
           # what is the area of this sa1 as a pct of the total area of the postcode
           mutate( pct =  postcode_intersect_sa1_area / sa1_area ) %>%
           mutate( sa1_population_covered_by_pc = population * pct ) %>%
           group_by( postcode_2016) %>%
           # how much does this sa1's population contribute to the total postcode population
           mutate( population_weighted_pct = sa1_population_covered_by_pc /
                  sum( sa1_population_covered_by_pc, na.rm=TRUE)) %>%
           ungroup() %>%
           mutate( across(
                          c(ends_with('score'),
                            ends_with('decile')),
                          ~(.x * population_weighted_pct))) %>%
           # total the seifa score and decile for the postcode
           group_by( postcode_2016) %>%
           summarise(
                     irsad_score_weighted = sum( irsad_score ,na.rm=TRUE),
                     irsd_score_weighted = sum( irsd_score ,na.rm=TRUE),
                     economic_score_weighted = sum( economic_score ,na.rm=TRUE),
                     education_score_weighted = sum( irsad_score ,na.rm=TRUE),
                     irsad_decile_weighted = sum( irsad_decile ,na.rm=TRUE),
                     irsd_decile_weighted = sum( irsd_decile ,na.rm=TRUE),
                     economic_decile_weighted = sum( economic_decile ,na.rm=TRUE),
                     education_decile_weighted = sum( irsad_decile ,na.rm=TRUE)
                     ) %>%
           ungroup()

         ,

         # calculate the average seifa and decile across all the impinging SA1's
         postcode_sa1_avg =

           postcode_sa1 %>%
           group_by(  postcode_2016 ) %>%
           summarise( across( c(ends_with('score'), ends_with('decile')), mean, na.rm=TRUE),
                     .groups='drop'
                     ) %>%
           rename_at( c(vars( ends_with( 'score')), vars( ends_with('decile'))), ~str_replace_all(.x, "$", "_average"))

         ,

         output  =

           absmapsdata::postcode2016 %>%
           st_drop_geometry() %>%
           left_join( postcode_remote_pct_wide, by = 'postcode_2016') %>%
           left_join( postcode_best_remote, by = 'postcode_2016') %>%
           left_join( postcode_sa1_avg, by = 'postcode_2016') %>%
           left_join( postcode_sa1_weighted, by = 'postcode_2016') %>%
           select( postcode_2016,
                  ends_with('Australia'),
                  ra_code,
                  ra,
                  ra_digit2,
                  ends_with('weighted'),
                  ends_with('average')
           )
           ,

           output_write =

             output %>% write_csv( 'output/postcode_seifa_remoteness_all.csv')

)

