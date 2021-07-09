module Handler.Info where

import           Import
import           Settings.Runtime
import           Text.Shakespeare.Text
import           Util.Handler          (addDocScripts)

getInfoR :: Handler Html
getInfoR = do
    instanceAdmin <- runDB getInstanceAdminEmail

    defaultLayout $ do
        addDocScripts
        setTitle "Carnap - About"

        $(widgetFile "infopage")
        -- TODO : split out the stuff specifically relating to exercises

-- TODO remove submit option on these.
checker :: Int -> Text -> Text -> Text -> Text -> Text -> HtmlUrl url
checker n thetype sys opts goal proof =
        [hamlet|
        <div class="exercise">
            <span>example #{show n}
            <div data-carnap-type="#{thetype}" data-carnap-options="#{opts}" data-carnap-system="#{sys}" data-carnap-goal="#{goal}">
                #{strip proof}
        |]
    where strip = dropWhile (== '\n')

proofcheck n = checker n "proofchecker"

sequentcheck n = checker n "sequentchecker"

-- XXX function that strips text of indentation and line-numbers.
aristotleTheorem = [st|
Show: P\/-P
    Show: --(P\/-P)
        -(P\/-P):AS
        Show: -P
            P:AS
            P\/-P:ADD 5
        :ID 6 3
        P\/-P:ADD 4
    :ID 3 8
    P\/-P:DNE 2
:DD 10|]

pierceTheorem = [st|
  (P->Q)->P     :A/CI
     -P         :A/~E
        P       :A/CI
          Q     :A/~E
          P     :3 R
          -P    :2 R
        Q       :4-6 -E
     P->Q       :3-7 CI
     P          :8 1 CE
     -P         :2 R
  P             :2-10 -E
((P->Q)->P)->P  :1-11 CI |]

lemmonTheorem = [st|
[1]       ExAy(Kxy -> Fxy) P
[2]       AxEy(Kxy)        P
[1,3]     Ay(Kay -> Fay)   (1)a EII
[2]       Ey(Kay)          (2) UI
[2,5]     Kab              (4)b EII
[1,3]     Kab -> Fab       (3) UI
[1,2,3,5] Fab              (5) (6) TF
[1,2,3,5] EyFay            (7) EG
[1,2,3,5] ExEyFxy          (8) EG
[1,2,3]   ExEyFxy          [5](9) EIE
[1,2]     ExEyFxy          [3](10) EIE|]

comprehensionTheorem = [st|
Show EXAx(F(x)/\G(x)<->X(x))
   Show Ax(F(x)/\G(x)<->\y[F(y)/\G(y)](x))
       Show F(c)/\G(c)->\y[F(y)/\G(y)](c)
            F(c)/\G(c):AS
            \y[F(y)/\G(y)](c):ABS 4
       :CD 5
       Show \y[F(y)/\G(y)](c)->F(c)/\G(c)
            \y[F(y)/\G(y)](c):AS
            F(c)/\G(c):APP 8
       :CD 9
       F(c)/\G(c)<->\y[F(y)/\G(y)](c):CB 3 7
   :UD 11
   EXAx(F(x)/\G(x)<->X(x)):EG 2
:DD 13|]

russellTheorem = [st|
Show -ExAy(-F(y,y) <-> F(x,y))
    ExAy(-F(y,y)<->F(x,y))          :AS
    Show: -ExAy(-F(y,y) <-> F(x,y))
        Ay(-F(y,y)<->F(c_1,y))      :AS
        -F(c_1,c_1)<->F(c_1,c_1)    :UI 4
        Show:-F(c_1,c_1)
            F(c_1,c_1)              :AS
            F(c_1,c_1)->-F(c_1,c_1) :BC 5
            -F(c_1,c_1)             :MP 8 7
        :ID 7 9
        -F(c_1,c_1) -> F(c_1,c_1)   :BC 5
        F(c_1,c_1)                  :MP 11 6
        Show: -ExAy(-F(y,y) <-> F(x,y))
        :ID 6 12
    :ED 13 2 4
:ID 2 3|]

russellTheoremForallx = [st|
    ExAy(-Fyy <-> Fxy):AS
       Ay(-Fyy<->Fry):AS
       -Frr<->Frr:AE 2
            -Frr:AS
            Frr:<->E 3 4
            -Frr:R 4
       Frr:-E 4-6
       -Frr:<->E 7 3
            ExAy(-Fyy <-> Fxy):AS
            Frr:R 7
            -Frr:R 8
       -ExAy(-Fyy <-> Fxy):-I 9-11
    -ExAy(-Fyy <-> Fxy):EE 1 2-12
    ExAy(-Fyy <-> Fxy):R 1
-ExAy(-Fyy <-> Fxy):-I 1-14|]

russellTheoremCalgary = [st|
    ExAy(-Fyy <-> Fxy):AS
        Ay(-Fyy<->Fry):AS
        -Frr<->Frr:AE 2
             -Frr:AS
             Frr:<->E 3 4
             !?:~E 4 5
        --
             Frr:AS
             -Frr:<->E 3 8
             !?:~E 8 9
        !?:LEM 4-6 8-10
    !?:EE 1 2-11
-ExAy(-Fyy <-> Fxy):-I 1-12|]

inverseTheorem = [st|
Show: AX2EY2∀x∀y(X2(x,y) ↔ Y2(y,x))
  Show: ∀x∀y(X2(x,y) ↔ \w\v[X2(v,w)](y,x))
    Show: ∀y(X2(a,y) ↔ \w\v[X2(v,w)](y,a))
      Show: X2(a,b) -> \w\v[X2(v,w)](b,a)
        X2(a,b):AS
        \w\v[X2(v,w)](b,a):ABS2 5
      :CD 6
      Show: \w\v[X2(v,w)](b,a)-> X2(a,b)
        \w\v[X2(v,w)](b,a):AS
        X2(a,b):APP2 9
      :CD 10
      X2(a,b) <-> \x_1\x_2[X2(x_2,x_1)](b,a):CB 4 8
    :UD 12
  :UD 3
  EY2∀x∀y(X2(x,y) ↔ Y2(y,x)):EG2 2
:UD2 15|]

adjunctionTheorem = [st|
Show P->(Q->R):CD
    P:AS
    Show Q->R:CD
      Q:AS
      P/\Q->R:PR
      P/\Q:&I 2 4
      R:->O 5 6|]

axiomFiveTheorem = [st|
Show <>[]P->[]P  /0   :CD
  <>[]P          /0   :AS
  Show []P       /0   :ND
    Show P       /0-1 :DD
      []P        /0-2 :<>O 2
      P          /0-1 :[]O(5) 5|]

axiomBTheorem = [st|
Show <>[]P->P /0   :CD
  <>[]P       /0   :AS
  []P         /0-1 :<>O 2
  P           /0   :[]O(b) 3|]

barcanTheorem = [st|
Show Ax[]Fx->[]AxFx  /0   :CD
  Ax[]Fx             /0   :AS
  Show []AxFx        /0   :ND
    Show AxFx        /0-1 :UD
      Show Fa        /0-1 :DD
        []Fa         /0   :AO 2
        Fa           /0-1 :[]O 6|]

bisectorTheorem = [st|
AxAyAz(F(x,g(y,z)) ↔ h(x,y) = h(x,z)) :PR
Show AwAxAyAz(F(w,g(x,y))∧F(w,g(x,z)) → F(w,g(y,z)))
 Show AxAyAz(F(a,g(x,y))∧F(a,g(x,z)) → F(a,g(y,z)))
  Show AyAz(F(a,g(b,y))∧F(a,g(b,z)) → F(a,g(y,z)))
   Show Az(F(a,g(b,c))∧F(a,g(b,z)) → F(a,g(c,z)))
    Show F(a,g(b,c))∧F(a,g(b,d)) → F(a,g(c,d))
     F(a,g(b,c))∧F(a,g(b,d))             :AS
     F(a,g(b,c))                         :S 7
     F(a,g(b,d))                         :S 7
     AyAz(F(a,g(y,z)) ↔ h(a,y) = h(a,z)) :UI 1
     Az(F(a,g(b,z)) ↔ h(a,b) = h(a,z))   :UI 10
     F(a,g(b,c)) ↔ h(a,b) = h(a,c)       :UI 11
     F(a,g(b,d)) ↔ h(a,b) = h(a,d)       :UI 11
     F(a,g(b,c)) → h(a,b) = h(a,c)       :BC 12
     F(a,g(b,d)) → h(a,b) = h(a,d)       :BC 13
     h(a,b) = h(a,c)                     :MP 8 14
     h(a,b) = h(a,d)                     :MP 9 15
     h(a,c) = h(a,d)                     :LL 16 17
     Az(F(a,g(c,z)) ↔ h(a,c) = h(a,z))   :UI 10
     F(a,g(c,d)) ↔ h(a,c) = h(a,d)       :UI 19
     h(a,c) = h(a,d) → F(a,g(c,d))       :BC 20
     F(a,g(c,d))                         :MP 18 21
    :CD 22
   :UD 6
  :UD 5
 :UD 4
:UD 3|]

transitiveTheorem = [st|
Show P(a) within P(P(a))
 a within P(a):PR
 Ax(x in a -> x in P(a)):Def-S 2
 Show Ax(x in P(a) -> x in P(P(a)))
  Show b in P(a) -> b in P(P(a))
   b in P(a) :AS
   b within a:Def-P 6
   Ax(x in b -> x in a):Def-S 7
   Show b in P(P(a))
    Show Ax(x in b -> x in P(a))
     Show c in b -> c in P(a)
      c in b :AS
      c in b -> c in a:UI 8
      c in a :MP 12 13
      c in a -> c in P(a) :UI 3
      c in P(a):MP 14 15
     :CD 16
    :UD11
    b within P(a):Def-S 10
    b in P(P(a)):Def-P 19
   :DD 20
  :CD 9
 :UD 5
 P(a) within P(P(a)):Def-S 4
:DD 24|]

sequentDemo = [st|
{"ident":0,"label":"AxEy(F(x)/\\G(y)):|-:EyAx(F(x)/\\G(y))","rule":"ER","forest":[{"ident":1,"label":"AxEy(F(x)/\\G(y)) :|-: EyAx(F(x)/\\G(y)), Ax(F(x)/\\G(b))","rule":"AL","forest":[{"ident":2,"label":"Ey(F(a)/\\G(y)), AxEy(F(x)/\\G(y)) :|-: Ax(F(x)/\\G(b)), EyAx(F(x)/\\G(y))","rule":"EL","forest":[{"ident":3,"label":"F(a)/\\G(c), AxEy(F(x)/\\G(y)) :|-: Ax(F(x)/\\G(b)), EyAx(F(x)/\\G(y))","rule":"&L","forest":[{"ident":4,"label":"G(c), AxEy(F(x)/\\G(y)) :|-: EyAx(F(x)/\\G(y)), Ax(F(x)/\\G(b))","rule":"AR","forest":[{"ident":5,"label":"G(c), AxEy(F(x)/\\G(y)) :|-: EyAx(F(x)/\\G(y)), F(d)/\\G(b)","rule":"&R","forest":[{"ident":6,"label":"AxEy(F(x)/\\G(y)), G(c) :|-: EyAx(F(x)/\\G(y)),F(d)","rule":"AL","forest":[{"ident":7,"label":"Ey(F(d)/\\G(y)), G(c) :|-: F(d), EyAx(F(x)/\\G(y))","rule":"EL","forest":[{"ident":8,"label":"F(d)/\\G(e), G(c) :|-: F(d), EyAx(F(x)/\\G(y))","rule":"&L","forest":[{"ident":9,"label":"F(d), G(c) :|-: EyAx(F(x)/\\G(y)), F(d)","rule":"WR","forest":[{"ident":10,"label":"F(d), G(c) :|-: F(d)","rule":"WL","forest":[{"ident":28,"label":"F(d) :|-: F(d)","rule":"Ax","forest":[{"ident":29,"label":"","rule":"","forest":[]}]}]}]}]}]}]},{"ident":11,"label":"AxEy(F(x)/\\G(y)), G(c):|-:EyAx(F(x)/\\G(y)),G(b)","rule":"XR","forest":[{"ident":11,"label":"AxEy(F(x)/\\G(y)), G(c):|-:G(b),EyAx(F(x)/\\G(y))","rule":"ER","forest":[{"ident":12,"label":"AxEy(F(x)/\\G(y)), G(c):|-:G(b),Ax(F(x)/\\G(c))","rule":"AR","forest":[{"ident":13,"label":"AxEy(F(x)/\\G(y)),G(c) :|-: G(b), F(a)/\\G(c)","rule":"AL","forest":[{"ident":14,"label":"Ey(F(a)/\\G(y)),G(c) :|-:G(b), F(a)/\\G(c)","rule":"EL","forest":[{"ident":15,"label":"F(a)/\\G(d),G(c) :|-:F(a)/\\G(c), G(b)","rule":"&L","forest":[{"ident":16,"label":"F(a), G(c) :|-:G(b), F(a)/\\G(c)","rule":"&R","forest":[{"ident":17,"label":"G(c),F(a) :|-: G(b), G(c)","rule":"WL","forest":[{"ident":18,"label":"G(c):|-:G(b),G(c)","rule":"WR","forest":[{"ident":26,"label":"G(c):|-:G(c)","rule":"Ax","forest":[{"ident":27,"label":"","rule":"","forest":[]}]}]}]},{"ident":19,"label":"F(a), G(c):|-: G(b), F(a)","rule":"WL","forest":[{"ident":20,"label":"F(a):|-:G(b), F(a)","rule":"WR","forest":[{"ident":22,"label":"F(a):|-:F(a)","rule":"Ax","forest":[{"ident":25,"label":"","rule":"","forest":[]}]}]}]}]}]}]}]}]}]}]}]}]}]}]}]}]}
|]
