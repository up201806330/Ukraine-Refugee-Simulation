globals [
  show_family_links?
  transparent
  max_age
  max_refugee_number
  total_refugees_departed
  gender_list
  first_refugee_wave

  max_leaving_delay
  max_population
  min_population

  gdp_min
  gdp_max
  pop_un_min
  pop_un_max
]

;; agents
breed [refugees refugee]
breed [countries country]

;; refugee agent links
undirected-link-breed [family-links family-link]
family-links-own [separated?]

;; link between refugee and country with number of family members that arrived there
undirected-link-breed [family-at-links family-at-link]
family-at-links-own [n_family]

countries-own[
  first_update
  second_update
  gdp ; divided by 1k
  population_unreceptiveness
  population
  max_refugees
  is_starting_country?

  accepted_number
  refused_number
  reunions_number
]

refugees-own[
  age
  gender
  race
  target_country
  visited_countries
  likeliness_of_staying
  min_likeliness_of_staying
  family_size
  family_remaining

  moving?
  departed?
  arrived?
  leaving_delay

  openness_weight
  distance_weight
  family_distance_weight
  gdp_weight
]


to setup
  clear-all
  reset-ticks
  file-close-all

  setup-refugees
  setup-countries
  new-refugees
  setup-interface
  update-label

  reset-ticks
end

to setup-interface
  ; reset runtime interface
  set agression_level 10
  set mandatory_military false

  set show_family_links? false
  Toggle-show-links

  set transparent [0 0 0 0]
end

to setup-refugees
  set max_age 100
  set max_refugee_number refugee_population
  set total_refugees_departed 0
  set gender_list ["Male" "Female"]
end

to setup-countries
  set max_leaving_delay 1000
  set max_population 3000
  set min_population 200

  set gdp_min 500
  set gdp_max 3000
  set pop_un_min 30
  set pop_un_max 70

  let locations_list []
  let gdp_list []
  let population_unreceptiveness_list []
  let color_list []
  let population_list []

  ;; load input scenario from file
  let random_params? input_file = ""
  if not random_params? [
    file-open (word "inputs/" input_file)
    set locations_list file-read
    set gdp_list file-read
    set population_list file-read
    set population_unreceptiveness_list file-read
  ]

  let j 0

  let patches_list []
  ifelse random_params?[
    let patches_agentset n-of number_receiving_countries patches with [
      distancexy 0 0 > 4 and abs pxcor < (max-pxcor - 1) and
      abs pycor < (max-pycor - 1)
    ]
    set patches_list [self] of patches_agentset
  ][
    set patches_list map [
      coordinates -> patches with [
        pxcor = item 0 coordinates and
        pycor = item 1 coordinates
      ]
    ] locations_list
  ]
  foreach patches_list [
    this_patch -> (
      ask this_patch [
        ; in order to get patches by input file order

        set color_list lput ((3 + random-float 6) + (10 * random 14)) color_list
        if random_params? [
          set population_list lput (min_population + (random (max_population - min_population))) population_list
        ]

        sprout-countries 1 [
          set shape "box"
          set size 2
        ]
        set j j + 1
      ]
    )
  ]
  let total_population sum population_list

  let i 0
  repeat count countries [
    ask country i [
      set label max_refugees
      set color item i color_list

      set population item i population_list
      set max_refugees round (population / total_population * refugee_population * 0.9)

      ifelse random_params? [
        set gdp gdp_min + (random (gdp_max - gdp_min))
        set population_unreceptiveness pop_un_min + (random (pop_un_max - pop_un_min))
      ][
        set gdp item i gdp_list
        set population_unreceptiveness item i population_unreceptiveness_list
      ]

      set first_update false
      set second_update false
      set accepted_number 0
      set refused_number 0
      set is_starting_country? false
    ]
    set i i + 1
  ]
  create-countries 1 [
    set shape "house"
    set size 2
    set label population
    set color ((3 + random-float 6) + (10 * random 14))

    set population refugee_population
    set is_starting_country? true
  ]
end

;; called every tick
to go
  accept-refugee
  move-refugees
  review_refugees
  review-policies
  update-label
  tick
end

;; "creates" the refugees
to new-refugees
  create-refugees max_refugee_number [
    ; properties
    set moving? false
    set arrived? false
    set departed? false
    set likeliness_of_staying random 100
    set min_likeliness_of_staying random likeliness_of_staying
    set age random max_age
    set gender one-of gender_list
    set target_country nobody
    set visited_countries []
    set family_size max list ((random (max_family_size * 2)) - max_family_size) 0
    set family_remaining family_size
    set leaving_delay random max_leaving_delay
    set openness_weight 1 + random (max_openness_weight - 1)
    set distance_weight 1 + random (max_distance_weight - 1)
    set family_distance_weight 1 + random (max_family_distance_weight - 1)
    set gdp_weight 1 + random max_gdp_weight
    ; visuals
    set shape "person"
    ifelse gender = "Male"[
      set color 105
    ][
      set color 135
    ]
    if age < 18[
      set size 0.7
    ]
  ]

  ; creating family links between refugees
  ask refugees[
    ; select n random refugees, create links and update their remaining family_size
    let possible_family_members other refugees with [family_remaining > 0]

    ; if no more people left with family to link, return
    if count possible_family_members <= 0 [stop]

    set family_remaining 0
    let random_refugees possible_family_members
    if family_size <= count possible_family_members [
      set random_refugees (n-of family_size possible_family_members)
    ]

    create-family-links-with random_refugees [
      set color transparent
    ]
    ask random_refugees [
      set family_remaining family_remaining - 1
    ]
  ]
end

to review_refugees
  ask refugees[
    ; if refugee hasn't left yet
    if not arrived? and not moving? [
      if likeliness_of_staying > min_likeliness_of_staying[
        set likeliness_of_staying likeliness_of_staying - random-float 0.005
      ]
      ifelse likeliness_of_staying < agression_level[
        ; mandatory enrollment prevents young males from leaving
        ifelse ( mandatory_military = true) and (gender = "Male") and (age > 18) and (age < 65) [
          set moving? false
        ][
          set moving? true
        ]
      ][
        set moving? false
      ]
    ]
  ]


end

;; moves the refugees
to move-refugees
  ask refugees with [moving?] [
    ifelse leaving_delay > 0[
      set leaving_delay leaving_delay - 1
    ][
      ifelse target_country = nobody[
        choose_country
      ][
        let move_distance random-float 1
        face target_country
        forward move_distance

        if not departed? and move_distance > 0[
          set departed? true
          set total_refugees_departed total_refugees_departed + 1
        ]
      ]
    ]
  ]
end

;; reviews country policies based on parameters
to review-policies
  ask countries [
    if not is_starting_country? [
      if (accepted_number / max_refugees) > 0.5 [
        if(first_update != true) [
          set max_refugees round(max_refugees * (1 / (population_unreceptiveness / 100)))
          set first_update true
        ]
      ]
      if (accepted_number / max_refugees) > 0.75 [
        if(second_update != true) [
          set max_refugees round(max_refugees * (1 / (population_unreceptiveness / 100)))
          set second_update true
        ]
      ]
    ]
  ]
end

to choose_country
  ask refugees with [moving? and target_country = nobody] [
    let refugees_visited_countries visited_countries
    let family my-family-links
    let temp_openness_weight openness_weight
    let temp_distance_weight distance_weight
    let temp_gdp_weight gdp_weight
    let temp_family_distance_weight family_distance_weight
    let temp_family_size family_size
    let desired_list []
    let country_list []
    ask countries[
      let this_country self

      ifelse
      ; refugees can only move to a country other than their home
      not is_starting_country?
      ; refugees won't reattempt to enter a visited country
      and not (member? self refugees_visited_countries)
      ; if country is full nobody will choose it anymore
      and max_refugees - accepted_number > 0[

        ; max population_unreceptiveness - 70 = 30
        let openness_weighed temp_openness_weight * ((population_unreceptiveness - 70) / 30) * ((population_unreceptiveness - 70) / 30)

        let distance_to_refugee distance self
        let distance_weighed temp_distance_weight * (distance_to_refugee / 40) * (distance_to_refugee / 40)

        ; if a family-at-link exists between this country and refugee
        let family_count 0
        if in-family-at-link-neighbor? myself
        [
          ; family count is n_family of said link
          set family_count [n_family] of (family-at-link (who) ([who] of myself))
        ]

        ;if family_count > 0 [show [who] of myself show family_count]
        ; max distance = 5
        let family_distance_weighed 0
        if temp_family_size > 0 [
          set family_distance_weighed temp_family_distance_weight * ((temp_family_size - family_count) / temp_family_size) * ((temp_family_size - family_count) / temp_family_size)
        ]

        ; max (3000-gdp) / 100 = 25
        let gdp_weighed  temp_gdp_weight * ((3000 - gdp) / 2500) * ((3000 - gdp) / 2500)

        let value sqrt(distance_weighed + openness_weighed + family_distance_weighed + gdp_weighed)
        set desired_list lput value desired_list
        set country_list lput this_country country_list
      ][
          ; baseline desireness
          set desired_list lput 10000 desired_list
          set country_list lput this_country  country_list
        ]
      ]

      ; target country will be the one with lowest value in desired_list
      let desired_index min desired_list
      let desired_country position desired_index desired_list
      set target_country (item desired_country country_list)

      ; update family links
      if target_country != nobody [
        ask my-family-links [
          ; if both ends of the link have different target countries, they are separated
          if [target_country] of other-end != [target_country] of myself [
            set separated? true
            set color red
            set thickness 0.1
          ]
        ]
      ]
    ]
end

to accept-refugee
  let accepted? false
  ask refugees with [moving? and not (target_country = nobody)] [

    if distance target_country < 0.25[
      ;check accepted
      set visited_countries lput target_country visited_countries
      ask target_country[
        ifelse accepted_number < max_refugees[
          set accepted? true
          ;;set moving? false
          set accepted_number accepted_number + 1

          ; save number of refugees that have reunited with family members
          let link_to_target_country family-at-link (who) ([who] of myself)
          if not (link_to_target_country = nobody) and [n_family] of link_to_target_country > 0[
            set reunions_number reunions_number + [n_family] of link_to_target_country

          ]
        ]
        [set refused_number refused_number + 1]
      ]
      if not accepted? [
        set target_country nobody
        stop
      ]

      set moving? false
      set arrived? true
      set accepted? false

      ; update family links with reunions where needed
      ask my-family-links [
        let arrival_country_link [target_country] of myself

        ; Both family member and this refugee have same target countries
        if [target_country] of other-end = arrival_country_link[
          ifelse [moving?] of other-end[
            ; if one is moving, they are reuniting
            set color green
            set thickness 0.5
          ][
            ; if they have both arrived, they are reunited
            if [arrived?] of other-end [
              set separated? false
              set color transparent
            ]
          ]
        ]

        ; for each family member,
        ; update/create family-at-links as needed
        ask other-end [
          ; if family-at-link exists between family member -- target country
          ifelse in-family-at-link-neighbor? arrival_country_link
          [
            ; increment n_family
            ask family-at-link (who) ([who] of arrival_country_link) [
              set n_family n_family + 1
            ]
          ]
          [
            ; otherwise, create link and set n_family
            create-family-at-link-with arrival_country_link [
              set n_family 1
              set color transparent ;these links are never visible
            ]
          ]
        ]
      ]
    ]
  ]
end

to update-label
  ask countries [
    ifelse is_starting_country? [
      ifelse ticks > 1[
        set label round max_refugee_number - total_refugees_departed
      ][
        set label round max_refugee_number
      ]
    ][
      set label round max_refugees - accepted_number
    ]
  ]
end

to toggle-show-family-links
  ask family-links [
    set hidden? not show_family_links?
  ]
end
to Toggle-show-links
  toggle-show-family-links
  set show_family_links? (not show_family_links?)
end
@#$#@#$#@
GRAPHICS-WINDOW
281
27
696
443
-1
-1
12.33333333333334
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
18
383
214
416
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
49
582
223
616
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
7
540
40
690
agression_level
agression_level
0
100
10.0
1
1
NIL
VERTICAL

SLIDER
15
12
212
45
number_receiving_countries
number_receiving_countries
0
100
7.0
1
1
NIL
HORIZONTAL

SLIDER
14
88
214
121
refugee_population
refugee_population
200
3200
3200.0
500
1
NIL
HORIZONTAL

SWITCH
49
657
223
690
mandatory_military
mandatory_military
1
1
-1000

SLIDER
15
50
213
83
max_family_size
max_family_size
0
5
5.0
1
1
NIL
HORIZONTAL

BUTTON
49
619
222
652
Toggle-show-links
Toggle-show-links
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

PLOT
477
456
677
644
Family Reunions
Time
NIL
0.0
10.0
0.0
10.0
true
true
"" "ask countries with [not is_starting_country?][\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy ticks reunions_number\n]"
PENS

PLOT
684
456
873
645
Departed Refugees
Time
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -5825686 true "" "plot total_refugees_departed"

PLOT
269
652
469
841
Refused Refugees
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" "ask countries with [not is_starting_country?][\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy ticks refused_number\n]"
PENS

PLOT
477
653
675
841
Tries until accepted
Nr. Tries
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range 0 4\nset-plot-y-range 0 count refugees with [arrived?] + 1\nset-histogram-num-bars 4" ""
PENS
"default" 1.0 1 -2139308 true "" "histogram [length visited_countries] of refugees"

MONITOR
708
10
950
55
GDP
map [\n  x -> [gdp] of x\n] sort-on [who] countries with [not is_starting_country?]
17
1
11

MONITOR
709
63
952
108
Population Unreceptiveness
map [\n  x -> [population_unreceptiveness] of x\n] sort-on [who] countries with [not is_starting_country?]
17
1
11

MONITOR
709
115
953
160
Places Left
map [\n  x -> [round (max_refugees - accepted_number)] of x\n] sort-on [who] countries with [not is_starting_country?]
17
1
11

MONITOR
710
167
954
212
Population
map [\n  x -> [population] of x\n] sort-on [who] countries with [not is_starting_country?]
17
1
11

MONITOR
710
219
954
264
Distance origin country
map [\n  x -> [(round ((distance country number_receiving_countries) * 100)) / 100] of x\n] sort-on [who] countries with [not is_starting_country?]
3
1
11

SLIDER
177
130
210
304
max_gdp_weight
max_gdp_weight
1
100
25.0
1
1
NIL
VERTICAL

SLIDER
124
130
157
304
max_family_distance_weight
max_family_distance_weight
1
100
100.0
1
1
NIL
VERTICAL

SLIDER
70
130
103
305
max_openness_weight
max_openness_weight
1
100
25.0
1
1
NIL
VERTICAL

SLIDER
17
129
50
309
max_distance_weight
max_distance_weight
1
100
25.0
1
1
NIL
VERTICAL

PLOT
268
457
468
645
Accepted Refugees
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" "ask countries with [not is_starting_country?][\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy ticks accepted_number\n]"
PENS

INPUTBOX
18
317
214
377
input_file
1
1
0
String

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
