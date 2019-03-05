
(defparameter *continents*
; Australia is classified as a country
'(africa antarctica north_america south_america europe asia eurasia ))

(defparameter *countries*
; Updated from the Wikipedia List of Sovereign States article, 2009-11-11, 
; with some historic ones (e.g., Czechoslovakia) and common aliases thrown 
; in. We omit 'Georgia' because it is ambiguous with the US state and 
; 'Chad' as it's more likely (perhaps) to be the personal name. 
; Also making 'Cote d'Ivoire' only appear as 'Ivory Coast' due to 
; representational difficulty.
  '(afghanistan albania algeria america american_samoa andorra angola
    argentina armenia aruba ascension_island australia austria azerbaijan
    bahamas bahrain bangladesh barbados belarus belgium belize benin
    bhutan bessarabia bolivia bosnia bosnia_herzegovina botswana brazil
    brunei bulgaria burkina_faso burma burundi cambodia cameroon canada
    cape_verde cccp central_african_republic chile china colombia comoros
    costa_rica croatia cuba curacao cyprus czech_republic czechoslovakia
    democratic_republic_of_congo democratic_republic_of_the_congo denmark
    dijibouti dominica dominican_republic dutch_antilles east_timor
    east_germany ecuador egypt el_salvador england equatorial_guinea eritrea
    estonia ethiopia faeroe_islands fiji finland france french_antilles
    french_guiana french_polynesia gabon gambia germany ghana greece
    greenland grenada guadaloupe guatemala guinea guinea-bissau guyana
    haiti holland honduras hungary iceland india indonesia iran iraq ireland
    israel italy ivory_coast jamaica japan jordan kasakhstan kazakhstan kenya
    kiribati korea kosovo kuwait kyrgyzstan laos latvia lebanon lesotho liberia
    libya liechtenstein lithuania luxembourg macao macedonia madagascar
    malawi malasia malaysia maldives mali malta marshall_islands mauritania
    mauritius mexico micronesia moldovoa monaco mongolia montenegro morocco
    mozambique namibia nauru nepal netherlands new_caladonia new_caledonia 
    new_zealand nicaragua niger nigeria north_korea norway oman pakistan 
    palau palestine panama papua_new_guinea paraguay persia peru philippines 
    poland portugal puerto_rico qatar republic_of_the_congo rhodesia romania 
    russia rwanda saint_lucia samoa san_marino saudi_arabia scotland senegal 
    serbia seychelles sierra_leone singapore slovakia slovenia solomon_islands
    somalia south_africa south_korea spain sri_lanka sudan suriname
    swaziland sweden switzerland syria taiwan tajikistan tanzania thailand
    tibet tobago togo tonga trinidad tunisia turkey turkmenistan tuvalu
    uganda ukraine united_arab_emirates united_kingdom uruguay
    usa united_states_of_america ussr uzbekistan vanuatu venezuela 
    vietnam wales west_germany yemen yemen_arab_republic yugoslavia 
    zaire zambia zimbabwe ))

(defparameter *us-states*
; AL, CAL, KY, MA, and PA are included as state abbreviations only with a
; final appended period (e.g., AL_\. and AL\.) as they would be produced in 
; `make-logical-atom' from the Treebank input, since without period they may
; be  mistaken for personal names. Other abbreviations with a period, like
; "Ala.", may appear in Treebank with punctuation attached or separated
; off as additional "words"; the latter are handled in `guess-entity-type', 
; which looks at the entity-tag of the word *without* the period as a separate,
; final word. We also omit NEW_YORK, because when spelled out this is usually 
; the city.

'(al_\. al\. ala ala_\. ala\. alabama ak ak_\. ak\. alaska az az_\. az\. ariz 
  ariz_\. ariz\. arizona ar ar_\. ar\. ark ark_\. ark\. arkansas ca ca_\. ca\.
  cal_\. cal\. calif calif_\. calif\. california co co_\. co\. col col_\. col\.
  colorado ct ct_\. ct\. conn conn_\. conn\. connecticut de de_\. de\. del 
  del_\. del\. delaware dc d_\._c_\. d\.c\. fl fl_\. fl\. florida ga ga_\.
  ga\. georgia hi hi_\. hi\. hawaii id id_\. id\. idaho il il_\. il\. ill 
  ill_\. ill\. illinois in in_\. in\. ind ind_\. ind\. indiana ia ia_\. ia\. 
  iowa ks ks_\. ks\. kansas ky_\. ky\. kentucky la la_\. la\. louisiana 
  me me_\. me\. maine md md_\. md\. maryland ma_\. ma\. mass mass_\. mass\. 
  massachusetts mi mi_\. mi\. mich mich_\. mich\. michigan mn mn_\. mn\. 
  minn minn_\. minn\. minnesota ms ms_\. ms\. mississippi mo mo_\. mo\. 
  missouri mt mt_\. mt\. montana ne ne_\. ne\. nebr nebr_\. nebr\. nebraska
  ny ny_\. ny\. n_\._y_\. n\.y\. nc nc_\. nc\. n_\._c_\. n\.c\. north_carolina
  nd nd_\. nd\. n_\._d_\.  n\.d\. north_dakota oh oh_\. oh_\. ohio ok ok_\. 
  ok_\. oklahoma or or_\.  or_\. oregon pa_\.  pa_\. penn penn_\. penn_\. 
  pennsylvania ri ri_\. ri_\. r_\._i_\. r\.i\. rhode_island sc sc_\. sc_\. 
  s_\._c_\. s\.c\. south_carolina sd sd_\. sd_\. s_\._d_\. s\.d\. south_dakota
  tn tn_\. tn_\. tenn tenn_\. tenn_\. tennessee tx tx_\. tx_\. texas ut ut_\. 
  ut_\. utah vt vt_\. vt_\. vermont va va_\. va_\. virginia wa wa_\. wa_\. 
  wash wash_\. wash_\. washington wv wv_\. wv_\. w_\._v_\. w\.v\. west_virginia
  wi wi_\. wi_\. wisc wisc_\. wisc_\. wisconsin wy wy_\. wy_\. wyoming ))

(defparameter *us-cities*
; I selected the most familiar-sounding ones from the telephone book

'(birmingham huntsville montgomery fairbanks phoenix flagstaff tucson
  little_rock bakersfield fresno los_angeles oakland palm_springs
  palo_alto pasadena sacramento san_bernadino san_diego san_francisco
  san_hose santa_monica colorado_springs denver hartford new_haven
  wilmington washington fort_lauderdale fort_meyers gainesville jacksonville
  miami miami_beach orlando tampa west_palm_beach atlanta augusta gainsville
  savannah honolulu chicago evenston lansing springfield indianapolis
  south_bend davenport des_moines sioux_city kansas_city wichita lexington
  louisville baton_rouge lafayette new_orleans portland portsmouth baltimore
  boston hyannis springfield worcester detroit grand_rapids lansing duluth
  minneapolis rochester biloxi st_louis omaha las_vegas atlantic_city
  jersey_city morristown newark alburquerque albany binghamton buffalo
  corning ithaca new_york manhattan schenectady syracuse utica yonkers
  charlotte greensboro raleigh cincinnati cleveland columbus dayton toledo
  oklahoma_city tulsa eugene portland erie harrisburg lancaster philadelphia
  pittsburgh reading scranton williamsport charleston greensville knoxville
  memphis nashville austin beaumont dallas fort_worth houston laredo san_antonio
  salt_lake_city norfolk richmond roanoke bellevue seattle spokane tacoma
  madison milwaukee oshkosh ))

(defparameter *world-cities*
; Again from telephone book, filling in some obvious omissions (and possibly
; getting the spelling wrong?) Augmented to include the top 40 entries in
; Wikipedia's "List of Cities Proper by Population". (But Vatican_city
; is included here, rather than under *countries*)
  '(ottawa toronto montreal quebec_city vancouver edmonton calgary winnipeg
    saskatoon halifax buenos_aires canberra melbourne sydney vienna salzburg
    brussels brasilia havana rio_de_janeiro mexico_city santiago bogota
    panama_city lima beijing peking shanghai budapest kiev minsk moscow
    prague riga sofia st_\._petersburg saint_petersburg leningrad tblisi
    warsaw zagreb amsterdam surat riyadh
    copenhagen goteborg helsinki oslo reykjavik stockholm the_hague
    vilnius algiers cairo addis_ababa monaco paris marseille nice berlin
    frankfurt munich hamburg athens guatemala_city port_au_prince
    hong_kong bombay calcutta delhi new_delhi hyderabad mysore damascus
    karachi jakarta nicosia tehran baghdad haifa jerusalem tel_aviv
    florence milan rome vatican_city venice mumbai sao_paulo wuhan kinshasa chennai
    alexandria antioch babylon constantinople atlantis troy ; ANCIENT CITIES
    tokyo yokohama amman seoul tripoli kuala_lumpur acapulco kingston
    nassau casablanca antilles auckland christ_church ho_chi_minh_city
    managua lagos islamabad manila lisbon singapore bridgetown cape_town
    pretoria barcelona madrid seville bern geneva lucerne zurich taipei
    bangkok tunis ankara istanbul abu_dhabi bahrain dubai belfast dublin
    cardiff dhaka guangzhou lahore bangalore tianjin kolkata chongqing
    glasgow london edinburgh caracas hanoi amran belgrade ))

(defparameter *government-agencies*
'(fbi cia nsa secret_service icc irs ins nsf nih nasa dod arpa darpa faa fcc fda
  fha dea hud nasa sec veterans_administration us_postal_service ))

(defparameter *legislatures*
  '(senate house_of_representatives state_senate parliament
    house_of_lords house_of_commons congress national_assembly
    bundestag imperial_diet))

(defparameter *international-political-entities*
'(un u_\._n_\. nato n_\._a_\._t_\._o_\. unesco european_union e_\._u_\.) )

(defparameter *activist-organizations*
; just a few token entries
'(afl-cio aclu ama greenpeace ))

(defparameter *service-agencies*
; just a few token entries
'(red_cross international_red_cross amnesty_international unicef
  ymca ymha ywca ywha) )

(defparameter *criminal-organizations*
'(cosa_nostra kkk mafia ))

(defparameter *weekdays*
'(monday tuesday wednesday thursday friday saturday sunday) )

(defparameter *months*
'(january february march april may june july august september october november
  december ))

(defparameter *religious-holidays*
'(christmas xmas easter hanuka hanukka hanukkah passover rosh_hashanah
  yom_kippur ramadan ))

(defparameter *civic-holidays*
'(new_year thanksgiving labor_day independence_day ))

(defparameter *planets*
'(mercury venus mars jupiter saturn uranus neptune pluto) )

(defparameter *rivers*
; just some major entries
'(amazon_river colorado_river danube ganges genesee_river indus mekong
  mississippi_river nile rhine rubicon thames tiber tigris yangtze ))

(defparameter *companies*
; just a few token entries
'(aol aol_time_warner disney eastman_kodak ibm microsoft motorola honeywell 
  nec att xerox siemens boeing ge general_electric kodak sony fuji mobil 
  exxon exxon_mobil texaco gm ford chrysler honda toyota mazda bmw mercedes 
  volkswagen fleet_bank chase_manhattan_bank
  citibank visa mastercard american_express ))

(defparameter *tv-networks*
; just a few token entries; for future reference: exclude FOX, since then a
; person so-named may be mistaken for the network.
'(abc cbs nbc pbs) )

(defparameter *martial-arts*
; just some major entries
'(karate jujitsu ju-jitsu jujutsu ju-jutsu judo taekwondo) )

(defparameter *beverages*
; just a few token entries
'(coke pepsi coca_cola coca-cola evian perrier sprite bud budweiser) )

(defparameter *medicines*; (ones likely to be labelled NNP)
; just a few token entries
'(ampicillin aspirin asa acetaminophen dramamine novocain penicillin 
  tylenol ))

;; Note: chemicals and drugs like dna, rna, ddt, lsd, etc., are unlikely
;; to be marked NNP and so needn't be included 

(defparameter *us-presidents*
; some entries collected from Webster's pocket dictionary
'(barack_obama barack obama john_adams bush george_bush george_w_\._bush 
  jimmy_carter clinton bill_clinton 
  dwight_eisenhower eisenhower gerald_ford gerry_ford herbert_hoover 
  andrew_jackson jefferson thomas_jefferson lyndon_johnson jfk kennedy 
  john_f_\._kennedy lincoln abe_licoln abraham_lincoln james_madison 
  william_mckinley james_monroe nixon richard_nixon reagan ronald_reagan 
  fdr f_\._d_\._r_\. roosevelt franklin_delano_roosevelt theodore_roosevelt 
  taft william_taft william_howard_taft truman harry_truman harry_s_\._truman
  john_tyler van_buren martin_van_buren george_washington woodrow_wilson ))

(defparameter *political-leaders*
; just a few major ones
'(adenauer konrad_adenauer arafat yasser_arafat beaverbrook lord_beaverbrook
  balfour begin menachem_begin 
  ben-gurion david_ben-gurion bismarck otto_von_bismarck bolivar simon_bolivar 
  castro fidel_castro churchill winston_churchill cicero coriolanus 
  cromwell oliver_cromwell de_gaulle degaulle charles_de_gaulle 
  charles_degaulle disraeli benjamin_disraeli frederick_douglass 
  ben_franklin benjamin_franklin indira_gandhi gandhi mahatma_gandhi
  mohandas_gandhi mohandas_k_\._gandhi garibaldi guiseppe_garibaldi 
  gladstone alexander_hamilton john_hancock 
  ho_chi_minh kissinger henry_kissinger metternich ataturk thomas_more 
  mustafa_kemal_ataturk ralph_nader nasser gamal_abdel_nasser nehru 
  jawaharlal_nehru thomas_paine william_penn peres shimon_peres juan_peron 
  rajendra_prasad rabin itzak_rabin walter_reuther richelieu
  cardinal_richelieu sadat anwar_sadat anwar_el_sadat ariel_sharon 
  talleyrand u_thant margaret_thatcher tito josip_tito josef_tito 
  joseph_tito trudeau pierre_elliot_trudeau ))

(defparameter *supreme-court-justices*
; just a few
'(salmon_chase john_marshall earl_warren james_earl_warren 
  william_douglas william_o_\._douglas strom_thurmond ))

(defparameter *dictators*
; just a few infamous ones
'(franco francisco_franco hitler adolf_hitler ivan_the_terrible lenin 
  mussulini polycrates stalin saddam_hussein idi_amin) )

(defparameter *emperors*
; just a few major ones
'(ashoka ashoka_the_great marcus_aurelius caesar julius_caesar caligula 
  charlemagne constantine cyrus cyrus_the_great kurush kurush_the_great 
            ; Cyrus is the Latinized name of Persian emperor Kurush
  francis_ii hirohito julian justinian_i leopold_i leopold_the_great
  leopold_ii mao mao_tse-tung mao_ze-dung maria_theresa maximilian
  montezuma montezuma_ii nero ramses tiberius titus valerian
  vespasian yoshihito ))

(defparameter *conquerors*; I might want to lump emperors and coquerors
                          ; perhaps as *rulers*?
; just a few major ones; note -- "Alexander" isn't necessarily the Great!
'(alaric alexander_the_great attila genghis_khan napoleon bonaparte
  tamerlane ) )

(defparameter *kings* ; also some czars and sultans (rename to "monarchs"?)
; just a few major ones
'(agamemnon alfred_the_great charles_v croesus darius darius_the_great 
  edward_i edward_ii adward_iii edward_iv edward_v edward_vi edward_vii
  edward_viii farouk ferdinand_i ferdinand_v ferdinand_vi ferdinand_vii
  francis_i frederick_i frederick_ii frederick_the_great george_i
  george_ii george_iii george_iv george_v george_vi gustavus_adolphus
  henry_i henry_ii henry_iii henry_iv henry_v henry_vi henry_vii henry_viii 
  herod hussein james_i james_ii king_arthur louis_i louis_vi louis_ix
  louis_xi louis_xii louis_xiii louis_xiv louis_xvi louis_xvii louis_xviii
  midas nebuchadnezzar oedipus oscar_i oscar_ii otto_i peter_i 
  peter_the_great philip_ii philip_iv philip_v ptolemy_i pyrrhus 
  ramses ramses_ii ramses_iii richard_i richard_the_lion-hearted 
  richard_ii richard_iii saladin sennacherib solomon suleiman 
  suleiman_the_magnificent wenceslaus william_i william_the_conqueror 
  william_ii william_iii william_iv xerxes xerxes_i ))

(defparameter *queens*
; just a few major ones; can't include "Elisabeth", "Victoria" (common names!)
'(cleopatra catherine_the_great elisabeth_i elisabeth_ii queen_elisabeth
  neferteti queen_victoria ))

(defparameter *military-commanders*; mostly generals
; just a few token entries
'(mark_anthony marc_anthony marc_antony cornwallis crazy_horse
  custer george_armstrong_custer geronimo guevara che_guevara
  hannibal lafayette robert_e_\._lee 
  macarthur douglas_macarthur montgomery richard_montgomery 
  admiral_nelson patton george_patton pershing john_pershing
  pompey_the_great colin_powell rommel erwin_rommel william_t_\._sherman
  william_tecumseh_sherman sitting_bull spartacus tecumseh ))

(defparameter *adventurers*
; just a few token entries; "Columbus" is classified as US-city...
'(amundsen buffalo_bill buffalo_bill_cody daniel_boone christopher_columbus 
  cortez cortes davy_crockett david_crockett
  vasco_da_gama francis_drake amelia_earhart eric_the_red john_franklin 
  thor_heyerdahl captain_kidd lawrence_of_arabia lindbergh  charles_lindbergh 
  david_livingstone magellan fernando_magellan marco_polo walter_raleigh
  ))

(defparameter *sports-celebrities*
; just a few token entries 
'(hank_aaron andre_agassi arthur_ashe yogi_berra borg muhammad_ali 
  larry_bird bjorn_borg federer lance_armstrong
  jim_brown cassius_clay wilt_chamberlain wilt_the_stilt
  wilt_the_stilt_chamberlain ty_cobb
  nadia_comaneci nadia_komaneci jimmy_connors howard_cosssell
  jack_dempsey joe_dimaggio roy_emerson
  phil_esposito tony_esposito chris_evert 
  lou_gehrig pancho_gonzales
  scott_hamilton gordie_howe bobby_hull
  kareem_abdul_jabbar
  reggie_jackson magic_johnson jack_kemp billie_jean_king sandy_koufax
  jari_kurri rod_laver guy_lafleur mario_lemieux ivan_lendl
  sugar_ray_leonard joe_louis mickey_mantle
  john_mcenroe john_mc_enroe wayne_gretzky frank_mahovlich
  bruce_jenner florence_griffith_joyner rocky_marciano joe_namath
  navratilova martina_navratilova stan_mikita 
  john_newcombe bobby_orr jesse_owens arnold_palmer pele 
  rocket_richard cal_ripken
  jackie_robinson bill_russell casey_stengel sugar_ray_robinson 
  ken_rosewall babe_ruth
  tretiak vladislav_tretiak tiger_woods ))

(defparameter *criminals/outlaws*
; just a few token entries; Booth's full name?
'(booth john_wilkes_booth james_earl_bundy al_capone john_gotti captain_kidd 
  charles_manson charlie_manson abu_nidal jack_the_ripper jesse_james 
  lee_harvey_oswald ))

(defparameter *artists*
; just a few major ones
'(bosch hieronymus_bosch botticelli brueghel pieter_brueghel caravaggio
  cellini benvenuto_cellini cezanne paul_cezanne chagall dali da_vinci 
  leonardo_da_vinci leonardo degas edgar_degas de_kooning willem_de_kooning
  delacroix donatello duerer albrecht_duerer el_greco gainsborough 
  thomas_gainsborough gauguin giotto goya franz_hals winslow_homer 
  ingres kandisnky klee paul_klee reginald_marsh matisse henri_matisse
  michelangelo michelangelo_buonarroti monet claude_monet henry_moore
  murillo bartolome_murillo maxfield_parrish picasso pablo_picasso 
  pisarro camille_pisarro jackson_pollock raphael santi_raffaello
  sanzio_raffaello rembrandt rembrandt_van_rijn renoir auguste_renoir
  pierre_auguste_renoir diego_rivera norman_rockwell rodin auguste_rodin 
  henri_rousseau rubens peter_paul_rubens ryder albert_ryder
  john_singer_sargent seurat georges_seurat tintoretto il_tintoretto 
  titian joseph_turner van_dyck van_gogh vincent_van_gogh velazquez 
  vermeer jan_vermeer verocchio veronese whistler wyeth andrew_wyeth))

(defparameter *architects*
; just a few miscellaneous ones
'(corbusier le_corbusier charles_le_corbusier gropius walter_gropius
  frank_lloyd_wright eero_saarinen christopher_wren ))

(defparameter *show-biz-stars*
; just a few miscellaneous ones
'(woody_allen fred_astaire george_balanchine anne_bankcroft marlon_brando 
  brigitte_bardot p_\._t_\._barnum john_barrymore 
  humphrey_bogart george_burns richard_burton
  charlie_chaplin julia_child sean_connery tom_cruise tony_curtis 
  robert_de_niro marlene_dietrich doris_day kirk_douglass isadora_duncan
  douglas_fairbanks w_\._c_\._fields harrison_ford jodie_foster 
  clark_gable cary_grant helen_hayes audrey_hepburn catherine_hepburn 
  charlton_heston bob_hope anthony_hopkins harry_houdini boris_karloff 
  grace_kelly nicole_kidman burt_lancaster charles_laughton
  jerry_lewis gina_lollobrigida sophia_loren
  jane_mansfield dean_martin steve_martin marilyn_monroe paul_newman 
  david_niven laurence_olivier peter_o_\'_toole gregory_peck 
  tyrone_power debbie_reynolds burt_reynolds 
  arnold_schwarzenegger sylvester_stallone ed_sullivan
  elisabeth_taylor
  john_wayne johnny_weissmueller
  ))

(defparameter *film-makers*
; some of the most famous ones
'(robert_altman ingmar_bergman 
  bernardo_bertolucci
  frank_capra 
  francis_ford_coppola
  walt_disney
  sergei_eisenstein
  fassbinder
  frederico_fellini
  milos_forman
  sam_goldwyn
  werner_herzog
  alfred_hitchcock
  constantin_costa-gavras
  stanley_kubrick
  spike_lee
  satyajit_ray
  roberto_rossellini
  steven_spielberg
  oliver_stone
  francois_truffant
  orson_welles
  franco_zefferelli
  ))

(defparameter *composers*
; just some major ones
'(albeniz isaac_albeniz bach j_\._s_\._bach johann_sebastian_bach 
  bartok bela_bartok beethoven ludwig_van_beethoven irving_berlin 
  bernstein leonard_bernstein bizet georges_bizet boccherini 
  luigi_boccherini borodin alexandr_borodin alexander_borodin brahms 
  johannes_brahms brecht bertold_brecht britten benjamin_britten 
  bruckner anton_bruckner byrd william_byrd chopin frederic_chopin 
  copeland aaron_copeland corelli arcangelo_corelli couperin 
  francois_couperin debussy claude_debussy delius frederick_delius
  dohnanyi ernst_von_dohnanyi dowland john_dowland dvorak antonin_dvorak 
  anton_dvorak antonyn_dvorak elgar edward_elgar duke_ellington 
  enesco georges_enesco de_falla manuel_de_falla cesar_franck frescobaldi
  girolamo_frescobaldi gershwin george_gershwin orlando_gibbons
  william_gilbert philip_glass grieg edvard_grieg handel
  george_frederick_handel george_friderick_handel georg_friederich_handel 
  georg_friederich_haendel haydn franz_joseph_haydn victor_herbert 
  holst gustav_holst humperdinck charles_ives scott_joplin
  kodaly zoltan_kodaly lehar franz_lehar liszt franz_liszt mahler 
  gustav_mahler mendelssohn felix_mendelssohn mozart wolfgang_amadeus_mozart
  moussorgsky offenbach jacques_offenbach orff carl_orff paganini
  nicolo_paganini palestrina cole_porter poulenc francis_poulenc prokofiev
  sergei_prokofiev prokofieff sergei_prokofieff puccini giacomo_puccini
  purcell henry_purcell ravel maurice_ravel respighi ottorino_respighi 
  rimsky-korsakov rossini gioacchino_rossiniscarlatti saint_saens
  camille_saint_saens charles_camille_saint_saens domenico_scarlatti 
  franz_schubert schumann robert_schumann shostakovich sibelius 
  jean_sibelius jan_sibelius strauss johann_strauss richard_strauss 
  stravinsky igor_stravinsky arthur_sullivan susa john_philip_susa suza 
  john_philip_suza tchaikovsky peter_tchaikovsky petr_ilich_tchaikovsky 
  teleman georg_philipp_teleman vaughan_williams ralph_vaughan_williams 
  vivaldi verdi guiseppe_verdi wagner richard_wagner ))

(defparameter *humanitarians*
; just a few
'(edith_cavell florence_nightingale mother_theresa albert_schweitzer
 ))

(defparameter *philosophers*
; just a few major ones
'(aristotle bacon francis_bacon george_berkeley confucius democritus
  descartes rene_descartes dewey john_dewey diogenes erasmus euclid frege 
  gottlob_frege hegel heidegger hobbes thomas_hobbes kant immanuel_kant 
  kongzi laozi leibnitz gottfried_wilhelm_von_leibnitz locke john_locke 
  maimonides marx karl_marx nietzsche friedrich_wilhelm_nietzsche occam 
  ockham william_of_ockham peirce charles_sanders_peirce plato pythagoras 
  quine russell bertrand_russell jean_jacques_russeau sartre jean_paul_sartre
  schopenhauer arthur_schopenhauer albert_schweitzer socrates oswald_spengler 
  voltaire whitehead alfred_north_whitehead wittgenstein ludwig_wittgenstein
  zeno zhuangzi ))

(defparameter *conductors*
; just a few major figures
'(barberolli sir_john_barberolli beecham thomas_beecham sir_thomas_beecham
  karl_bohm karl_boehm karajan herbert_von_karajan andre_previn 
  george_szell toscanini arturo_toscanini ))

(defparameter *singers*
; just a few major figures
'( marian anderson
   joan_baez
   david_bowie ziggy_stardust
   enrico_caruso
   nat_king_cole
   bing_crosby
   fats_domino
   bob_dylan
   aretha_franklin
   woodie_guthrie
   billie_holiday
   buddy_holly
   john_lee_hooker
   michael_jackson
   b_\._b_\._king
   peggy_lee
   john_lennon
   paul_mc_cartney paul_mccartney
   joni_mitchell
   ricky_nelson
   elvis_presley elvis
   otis_redding
   little_richard
   paul_robeson
   jimmy_rodgers
   pete_seeger
   paul_simon
   frank_sinatra
   bruce_springsteen
   rod_stewart
   sting
   joan_sutherland
   james_taylor
   muddy_waters
   hank_williams
   stevie_wonder
   frank_zappa

  ))

(defparameter *musicians*
; just a few major figures
'(louis_armstrong
  chet_atkins
  alfred_brendel
  jack_benny
  casadesus
  pablo_casals
  miles_davies
  jimmy_dorsey
  walter_gieseking
  fritz_kreisler
  manitas_de_plata
  yehudi_menuhin
  sophie_mutter
  david_oistrakh igor_oistrakh
  itzak_perlman
  sviatoslav_richter
  artur_rubenstein
  arthur_schnabel
  andres_segovia
  rudolf_serkin
  isaac_stern
  ))

(defparameter *bands*
; just a few major ones, sel. from
;  http://www.rockhall.com/hof/allinductees.asp?sort=ln%2Cfn
'(
  ace_of_base
  aerosmith
  allman_brothers_band
  beatles
  crosbie_\,_stills_and_nash
  fleetwood_mac
  jefferson_airplane
  pink_floyd
  rem rolling_stones
  santana
  simon_and_garfunkel
  the_beach_boys beac_boys 
  the_bee_gees bee_gees
  the_doors
  the_everly_brothers everly_brothers
  the_grateful_dead grateful_dead
  the_supremes
  the_who
  led_zeppelin  ))

(defparameter *scientists*
; just some of the major figures
'(archimedes arrhenius babbage charles_babbage alexander_graham_bell 
  bernoulli daniel_bernoulli bohr niels_bohr copernicus nicholaus_copernicus 
  nicolaus_copernicus nicolas_copernicus curie marie_curie pierre_curie 
  dalton john_dalton darwin charles_darwin de_broglie louis_de_broglie 
  democritus dirac paul_dirac edison thomas_edison einstein albert_einstein 
  euler leonhard_euler leonard_euler faraday michael_faraday
  fermat fermi enrico_fermi feynman richard_feynman alexander_fleming
  freud sigmund_freud galen claudius_galen galileo gauss 
  karl_friedrich_gauss hahn otto_hahn hawking stephen_hawking 
  heisenberg werner_heisenberg helmholtz hermann_von_helmholtz
  hero_of_alexandria heron_of_alexandria hippocrates hubble edwin_hubble
  von_humboldt friedrich_von_humboldt julian_huxley huyghens
  christian_huyghens william_james edward_jenner frederic_joliot-curie
  irene_joliot-curie jung carl_jung william_kelvin lord_kelvin kepler 
  johann_kepler johannes_kepler kirchhoff gustav_robert_kirchhoff
  lagrange joseph_louis_lagrange laplace pierre_laplace legendre 
  linnaeus marconi guglielmo_marconi maxwell james_clark_maxwell 
  mead margaret_mead mendel gregor_mendel samuel_morse
  newton isaac_newton oppenheimer robert_oppenheimer paracelsus
  pascal blaise_pascal pasteur louis_pasteur pauli wolfgang_pauli 
  pauling linus_pauling pavlov planck poincare henri_poincare 
  jules_henri_poincare max_planck ptolemy roenthgen ronthgen
  wilhelm_roenthgen ernest_rutherford sagan carl_sagan jonas_salk 
  schliemann heinrich_schliemann schroedinger erwin_schroedinger 
  teller edward_teller turing alan_turing vasalius andreas_vasalius 
  von_neumann john_von_neumann 
  watt james_watt weinberg john_wheeler norbert_wiener yukawa ))

(defparameter *engineers/inventors*
; a few miscellaneous ones
'(babbage charles_babbage werner_von_braun goddard rudolf_diesel
  gutenberg johann_gutenberg orville_wright wilbur_wright ))

(defparameter *economists*
; a few miscellaneous ones
'(john_galbraith john_kenneth_galbraith milton_friedman john_maynard_keynes 
  adam_smith thorsten_veblen ))

(defparameter *writers*
; many of those who are "household words"
'(aesop edward_albee hans_christian_anderson asimov isaac_asimov austen 
  jane_austen balzac honore_de_balzac beckett samuel_beckett boccaccio 
  bronte emily_bronte charlotte_bronte boswell james_boswell browning 
  robert_browning robert_burns samuel_butler george_byron lord_byron 
  erskine_caldwell capek karel_capek capote truman_capote dale_carnegie
  cervantes miguel_cervantes chaucer geoffrey_chaucer 
  chekhov anton_chekhov coleridge samuel_taylor_coleridge
  joseph_conrad noel_coward dante dante_alighieri dickens 
  charles_dickens diderot denis_diderot dinesen isak_dinesen 
  blixen karen_blixen emily_dickinson donne john_donne dostoyevsky 
  dostoevski feodor_dostoyevsky feodor_dostoevski arthur_conan_doyle 
  dryden john_dryden dumas alexandre_dumas alexander_dumas eco umberto_eco 
  george_eliot mary_ann_evans t_\._s_\._eliot emerson ralph_waldo_emerson 
  faulkner william_faulkner fielding henry_fielding e_\._m_\._forster
  frost robert_frost christopher_fry john_galsworthy goethe wolfgang_goethe
  wolfgang_von_goethe robert_graves  graham_greene  zane_grey 
  grillparzer franz_grillparzer grimm the_grimm_brothers 
  the_brothers_grimm thomas_hardy heinrich_heine hemingway
  ernest_hemingway herodotus james_hilton homer victor_hugo ibsen 
  henrik_ibsen aldous_huxley henry_james james_joyce kafka franz_kafka 
  keats john_keats helen_keller omar_khayyam stephen_king kipling 
  rudyard_kipling koestler arthur_koestler lagerlof 
  lagerloef selma_lagerlof selma_lagerloef lessing
  gotthold_lessing gotthold_ephraim_lessing doris_lessing jack_london
  longfellow henry_wadsworth_longfellow macauley thomas_macauley machiavelli 
  niccolo_machiavelli norman_mailer thomas_mann marlowe christopher_marlowe
  maugham somerset_maugham william_somerset_maugham marshall_mcluhan
  melville herman_melville h_\._l_\._mencken james_michener henry_miller
  milton john_milton moliere iris_murdoch nabokov vladimir_nabokov 
  ogden_nash nostradamus john_o_\'_hara john_henry_o_\'_hara eugene_o_\'_neill 
  orwell george_orwell ossian ovid john_payne charles_perrault petrarch
  plutarch poe edgar_allan_poe alexander_pope katherine_anne_porter
  ezra_pound j_\._b_\._priestley marcel_proust pushkin alexandr_pushkin
  alexander_pushkin ellery_queen rabelais francois_rabelais rilke
  rainer_maria_rilke hans_sachs sade marquis_de_sade j_\._d_\._salinger
  schiller friedrich_schiller sir_walter_scott walter_scott shakespeare 
  william_shakespeare shaw george_bernard_shaw shelley isaac_singer
  solzhenitsyn alexandr_solzhenitsyn alexander_solzhenitsyn sophocles
  steinbeck john_steinbeck robert_louis_stevenson theodor_storm
  harriet_beecher_stowe jonathan_swift tacitus tagore rabindranath_tagore
  sir_rabindranath_tagore tennyson alfred_\,_lord_tennyson thackeray 
  william_thackeray dylan_thomas thoreau henry_david_thoreau tolstoy 
  leo_tolstoy lev_tolstoy toynbee arnold_toynbee turgenev mark_twain clemens 
  samuel_clemens jules_verne whitman walt_whitman walter_whitman oscar_wilde 
  tennessee_williams p_\._g_\._wodehouse virginia_woolf wordsworth
  william_wordsworth herman_wouk yeats william_butler_yeats ))

(defparameter *literary-works*
; just a few token entries; Don Quixote and Moby Dick would be here if
; they weren't also characters! Likewise Macbeth, Julius Caesar, etc.
'(a_christmas_carol arabian_nights bible the_bible the_holy_bible 
  crime_and_punishment decameron devine_comedy east_of_eden
  the_grapes_of_wrath gulliver_\'_s_travels iliad inferno koran 
  new_testament odyssey old_testament the_old_man_and_the_sea 
  romeo_and_juliet talmud torah veda vedas war_and_peace ))

(defparameter *tycoons*
; a few miscellaneous ones
'(george_eastman john_henry_ford bill_gates randolph_hearst 
  john_d_\._rockefeller nelson_rockefeller cornelius_vanderbilt
 ))

(defparameter *famous-lawyers*
; just a couple
'( clarence_darrow daniel_webster))

(defparameter *popes*
; just a few miscellaneous ones
'(adrian_i adrian_iv alexander_vi benedict_i benedict_xv boniface_iv 
  callixtus_iii clement_vii john_paul_ii leo_xiii paul_i paul_iii paul_v 
  paul_vi pius_v pius_x pius_xi pius_xii urban urban_i urban_viii
 ))

(defparameter *religious-leaders*
; just a few token entries
'(aquinas thomas_aquinas buddha john_calvin george_fox gautama gotama 
  herzl theodor_herzl joan_of_arc luther martin_luther mahomet mohammed 
  muhammad mary_baker_eddy moses savanarola joseph_smith spinoza
  john_wesley xavier francis_xavier brigham_young zwingli ))

(defparameter *fictitious-characters*
; just some major entries
'(achilles boogie_man cyclops d_\'_artagnon delilah don_juan don_quixote 
  electra frankenstein gulliver helen_of_troy hercules heraklites sherlock_holmes 
  robin_hood indiana_jones lancelot methuselah pegasus perseus prometheus 
  odysseus santa santa_claus samson spiderman superman tarzan theseus ulysses ))

(defparameter *deities*
'(aphrodite apollo athena bacchus brahma dyonesus god he him himself juno 
  jupiter allah the_almighty god_almighty the_lord jehovah jahweh jahwe 
  jesus christ jesus_christ krishna lucifer satan siva) )
