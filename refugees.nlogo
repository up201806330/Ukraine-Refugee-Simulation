globals [
  ; interface
  show_family_links?
  transparent
  max_leaving_delay
  max_age
  max_refugee_number
  total_refugees_departed
  color_list
  population_list
  total_population
  ;total_GDP
  ;GDP_list
  ;acceptance_ratio_list
  max_population
  min_population
  gender_list

  distance_weight
  openness_weight
  family_distance_weight
]

;; agents
breed [refugees refugee]
breed [countries country]

;; refugee agent links
undirected-link-breed [family-links family-link]
family-links-own [separated?]

;; link between refugee and country with number of family members that arrived there
undirected-link-breed [family-country-links family-country-link]
family-country-links-own [n_family]

countries-own[
  first_update
  second_update
  policy_generosity
  accepted_number
  population_unreceptiveness
  population
  max_refugees
  is_starting_country? ; the country who generates the refugees
]

refugees-own[
  age
  gender
  race
  moving?
  target_country
  visited_countries
  likeliness_of_staying
  family_size
  family_remaining
  arrived?
  leaving_delay
]


to setup
  clear-all
  setup-refugees
  setup-countries
  new-refugees
  setup-interface

  reset-ticks
end

to setup-interface
  set show_family_links? true
  Toggle-show-links

  set transparent [0 0 0 0]
end

to setup-refugees
  set max_age 100
  set max_refugee_number refugee_population
  set total_refugees_departed 0
  set gender_list ["Man" "Woman"]
  set distance_weight 10
  set openness_weight 5
  set family_distance_weight 30
end

to setup-countries
  set max_leaving_delay 300
  set max_population 50000
  set min_population 2500
  set color_list []
  set population_list []
  ;set GDP_list []


  ask n-of number_receiving_countries patches with [
    distancexy 0 0 > 4 and abs pxcor < (max-pxcor - 1) and
    abs pycor < (max-pycor - 1)
  ] [
    ; randomly placing hives around the center in the
    ; view with a minimum distance of 16 from the center
    set population_list lput (min_population + (random (max_population - min_population))) population_list
    set color_list lput ((3 + random-float 6) + (10 * random 14)) color_list
    ;set GDP_list lput (32 + random (277 - 32)) GDP_list
    sprout-countries 1 [
      set shape "box"
      set size 2
      ;set policy_generosity 0
      ;set accepted_number 0
      ;set population_unreceptiveness false
    ]
  ]
  set total_population sum population_list

  let i 0 ; assign quality and plot pens to each hive
  repeat count countries [
    ask country i [
      set label max_refugees
      set color item i color_list

      set population item i population_list
      set max_refugees round (population / total_population * refugee_population * 0.5)
      set population_unreceptiveness ((random 30) + 70)
      set first_update False
      set second_update False
      set accepted_number 1
      set is_starting_country? false
    ]
    ;set-current-plot "on-site"
    ;create-temporary-plot-pen word "site" i
    ;set-plot-pen-color item i color-list
    ;set-current-plot "committed"
    ;create-temporary-plot-pen word "target" i
    ;set-plot-pen-color item i color-list
    set i i + 1
  ]
  create-countries 1 [
    set shape "house"
    set size 2
    set label population
    set color ((3 + random-float 6) + (10 * random 14))

    set population refugee_population
    set accepted_number 1
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
    set arrived? false
    set likeliness_of_staying random 100
    set age random max_age
    set gender one-of gender_list
    set target_country nobody
    set visited_countries []
    set family_size max list ((random max_family_size * 2) - max_family_size) 0
    set family_remaining family_size
    set leaving_delay random max_leaving_delay
    ; visuals
    set shape "person"
    ifelse gender = "Man"[
      set color 105
    ][
      set color 135
    ]
    if age < 18[
      set size 0.7
    ]

    ; first tick of updating properties
    ifelse likeliness_of_staying < agression_level[
      set moving? true
    ][
      set moving? false
    ]
    if( mandatory_enrollment = true) and (gender = "Man") and (age > 18) and (age < 65) [
      set moving? false
    ]
  ]

  ; creating family links between refugees
  ask refugees[
    ; select n random refugees, create links and update their remaining family_size
    let possible_family_members other refugees with [family_remaining > 0]

    ; if no more people left with family to link, return
    if count possible_family_members <= 0 [stop]

    set family_remaining 0
    let random_refugees (n-of family_size possible_family_members)

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
      ; set likeliness_of_staying likeliness_of_staying - 0.1
      ifelse likeliness_of_staying < agression_level[
        ; mandatory enrollment prevents young males from leaving
        ifelse ( mandatory_enrollment = true) and (gender = "Man") and (age > 18) and (age < 65) [
          set moving? false
        ][
          set moving? true
          set total_refugees_departed total_refugees_departed + 1
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
        face target_country
        forward random-float 1
      ]
    ]
  ]
end

;; reviews country policies based on parameters
to review-policies
  ask countries [
    if not is_starting_country? [
      if (accepted_number / max_refugees) > 0.5 [
        if(first_update != True) [
          set max_refugees (max_refugees * (1 / (population_unreceptiveness / 100)))
          set first_update True
        ]
      ]
      if (accepted_number / max_refugees) > 0.75 [
        if(second_update != True) [
          set max_refugees (max_refugees * (1 / (population_unreceptiveness / 100)))
          set second_update True
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
        if accepted_number < max_refugees[
          set accepted? true
          ;;set moving? false
          set accepted_number accepted_number + 1
        ]
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
        ; Both family member and this refugee have same target countries
        if [target_country] of other-end = [target_country] of myself[
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
      ]
      ; update/create family-country-links as needed
    ]
  ]
end

to choose_country
  ask refugees with [moving? and target_country = nobody] [
    let refugees_visited_countries visited_countries
    let family my-family-links

    let desired_list []
    ask countries[
      let this_country self

      ifelse
      ; refugees can only move to a country other than their home
      not is_starting_country?
      ; refugees won't reattempt to enter a visited country
      and not (member? self refugees_visited_countries)[

        let openness_weighed openness_weight * population_unreceptiveness * population_unreceptiveness

        let distance_to_refugee distance self
        let distance_weighed distance_weight * distance_to_refugee * distance_to_refugee

        ;let family_count count family with [
        ;  ; count family members that arrived to this country
        ;  [arrived?] of other-end and
        ;  this_country = [target_country] of other-end
        ;]
        ;let family_distance_weighed family_distance_weight * family_count * family_count

        let value sqrt(distance_weighed + openness_weighed)
        set desired_list lput value desired_list
      ][
        ; baseline desireness
        set desired_list lput 1000 desired_list
      ]
    ]

    ; target country will be the one with lowest value in desired_list
    let desired_index min desired_list
    let desired_country position desired_index desired_list
    set target_country country desired_country

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

to update-label
  ask countries [
    ifelse is_starting_country? [
      set label round max_refugee_number - total_refugees_departed
    ][
      set label round max_refugees - accepted_number + 1
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
266
10
703
448
-1
-1
13.0
1
10
1
1
1
0
1
1
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
16
130
212
163
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
48
332
220
365
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
6
291
39
441
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
2500
50000
2500.0
500
1
NIL
HORIZONTAL

PLOT
229
502
429
690
Accepted Refugees
Time
Number Accepted Refugees 
0.0
10.0
0.0
10.0
true
true
"" "ask countries [\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy ticks accepted_number\n]"
PENS

SWITCH
48
407
222
440
mandatory_enrollment
mandatory_enrollment
0
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
3.0
1
1
NIL
HORIZONTAL

BUTTON
48
369
221
402
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
NetLogo 6.0.4
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
