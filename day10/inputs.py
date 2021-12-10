sample = '''[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]'''.split('\n')

full = '''
[<([((<(<<{{{[{}[]][<>{}]}[{[]()}{[]<>}]}<({(){}}<()<>>](<{}<>><[]<>>)>}<<[<<><>>[()<>]]{({}<>)
<([{<<{[{{{[[{{}[]}(<>[])]{[[]<>]>]<[[<><>]([]<>)]{<{}<>>{{}<>}}>}[<((<>())[[]{}])>[<{[][]}<{
[{{[{([[[[{[{((){})(()<>)}{([]())}][(({}{})[[]()]){(<>{})<()<>>}]}][[{<{[]{}}[{}()]>[[{}<>](<>{})]}[
[{[<[(([{[<(<{{}{}}([]<>)>)>([[[[]{}]<(){}>](<{}[]>[[]{}])]({<{}{}>}<{[]()}([][])>))]}{{[[[<[]{}>([]())]{{
{(<[<{(<[(({([{}[]]({}{}))<{()[]}([][])>})([[<{}[]>([])][<[]{}><<><>>]][[<<>[]><(){}>](({}{}){<>()})]))
<{(<[[([[[{<[({}[])<()()>]>}[[{<<>()><{}()>}[<<>{}><[]<>>]]]][(<({[]}([][]))><(<{}{}>){({}())[[]{}]}>){[[
(<(<(((<<{[<[{[]{}}({}{}>]><[<()<>><{}{}>][{()<>}({}())]>]}({<[[[]()]{[]<>}](<[]<>>([]<>))>[{(<><>)([][])}]
<{[[{({<([[<{(<>())[<>[]]}><({[]{}}<<>>)[((){})[[]()]]>]]({<<(<><>)[{}<>]>{[{}{}](()[])}>([([]<>){()
(((<{{{{<<[<([[]<>])({()<>}[{}<>])>(<{<>()}>{[{}<>]})]><[[([[]{}]{{}()})(([]){(){}})]{[{<>{}}<<>{}>
[{((({[[([[[{{[]<>}(<>[])}[([]{})([][])}]<[(<><>){[][]}]{<{}{}>(()())}>]]<([<{{}()}{{}<>}>[(
{(<{[[{(<[[{{[(){}](()[])}}<[(<>{}){[][]}]{<<><>>{{}()}}>][{{{()}<()()>}{<<>()>(()[])}}]]>[{<({[<>
(([([([(([<([([][])<()()>]<({}()]({}())>)>{[{<[][]>(<>{})}<{{}{}}(<>{})>]<{([]())(<>{})}[{<>{}}[()()]]
{<<<{[({(<({(<{}()>){[<>()](()<>)}})<[<{()[]}{<><>}>]{[[(){}]({}<>)][<<>{}><{}<>>]}>><(([<{}[]>(<>
{[[([[([{[(<<(<>[]){{}}><([]<>)<()<>>>><([(){}](<>{}))[{[]}<{}<>>]>)][{{(({}())[[]{}])}{([()<>][[]<>]){<[][]>
{([{(({<<{<{{(<>())[{}{}]}{(()[])[[]<>]}}>}>[<{(<[[]()]<{}()>>)<{((){}]((){})}[[{}]([]{})]>}([({
(([(<({[<({{<{<>[]}<<>{}>](([]()))}<(<{}()>(<>))<((){})[<>{}]>>})({[[<()<>>][<()<>>[<><>]]](({
<<((<<{[{(<<<{{}{}}[{}<>])(<<>()>[[][]])>>([({()[]}[<>{}]){[{}<>]{()<>}}]{(<{}{}>[<>[]])[{<>[]}<[]{
([{(<<{{<{<<<<()<>>([][])>(([]()){()})><{[{}()]{<>{}}}<{()<>}<()[]]>>>}{<<<{()}((){})>[[{}()][<>]]>>[
<[(<<<([{{{((<()>(<>{}))<([]<>)(<>())>){([(){}]<()()>)}}{{[[()[]](()<>)]{(<>()){()()}}}<[({}()){[]<>}][<{}[
(<{{({({({[{([()<>])[{<>{}}]}{<([]())[[]<>]>([()()]{[]<>})}]{(<[{}()]<<>()>>[<<>())<()[]>])[({()()}[[]
<({<({[{(({{{<{}()>[(){}]}{({}<>)<[]{}>}}<<<{}[]>([]<>)>[(()())[()<>]]>}<(<[[]{}]([]())>([(){}]<[][]
{(((<(<{{{<[[{[][]}]<{[]{})[{}()]>]<[<{}<>>][[[]()](()<>)]>>}([([{{}[]}[<>[]]][(<>{}){[]()}])({[{}[]][<>
{{({(<{{(([<(<()<>>{(){}})>[<{()()}[<>{}]>[<<>{}>{[][]}]]]({{<<><>><[][]>}[[<>()]<[]()>>}<<(()())[[]<>]><{<
<{(<([{[{<[[[(<>)({}{})]]((<()<>>))]{{{(()<>)([])}}[{{{}()}{<>[]}}[{[]{}}{<><>}]]}>}][<[<[<[(){}]><({}[])<<>{
<({[[<<([(<{[<[]<>>[{}<>]](({}[]))}{{[{}<>]}([[]{}]([]{}))}>[[{<[]<>)<{}>}{{(){}}{()}}]([(<>{})]<<[]()>
{(([{<{(((<<<(()[])([]())>[<<>{}>({}[]]]>>[[([[]<>]<{}[]>)<[<>[]]{(){}}>][((<>[])(()[]))]]))(([{<{<>}{{}<>
<[((([<{{([{({<>{}}(<><>)){([]{})<()[]>}}({[[][]]([]{})}({[]<>}))][<<[<>{}]([][])>(<{}<>>[(
<[{{[{{<{[[([<<><>>]{({}())([]<>)})]]}[<({([{}{}]{[]})({()()}[(){}])}[{<(){}><{}[]>}])<[[<[]{}>
(<(<{<{<{([([<<><>>]({[]}[()[]])){<{<>()}<()[]>>[([]{}){{}[]}]}]{{({[]()}{<><>})([{}()](()()))}<[<[][]>][<[]
{[{{<[(<(<{<{{<><>}{{}{}}}([()()])>[<<{}>[()()]>[[()()]{[][]}]]}>)[<<<<<[]()><(){}>>[[<>()]<{}{}>]>[{{
([{(<({((([<(<()()><[]()>)({{}()}{<>()})>{[({}[]){{}[]}]{{{}()}{{}<>}}}])(<[{((){}){[][]}}([[]()])]{<{()<>
((<<[{<[(([({(()<>)[{}{}]}{<{}[]><[]<>>})<[<()>]([()<>])>]{((<[]{}><()<>>)<{{}><<>>>)[(<{}()>)<[[]()](
({<([[<{[{[[{{{}<>}({}())}<[<>()][{}[]]]][[{[]<>}]<([]{})[{}()]>]]}]([{[(<[]{}>[{}[]])]}]{((<[{}
<[(<{<[<<({<<{<><>}(()())>)}{(({(){}}){[{}[]]{{}{}}})})>>]>}<[<{(<<[(<()<>>[[]()]){{{}<>}<[][]>}][(<(
({([(<{[{[{<({<>{}}<()[]>)>{[(<><>){{}<>}]}}]}]}[([[{[[[[]{}]][([][]){<>{}}]][[{[][]}(<>[])]
[<([<[([[{{({{<>{}}[{}[]]}([()()]({})))((<(){}>)([[]())({}<>)))}}]])]>{<<(({{<[<{}[]>[()<>]]>}}([[{({}<>)
<{[{<[<([{(<{<<>{}><<>()>}[(<>[])[<><>]]>[(((){}))<(<>){{}[]}>]){(<({}{})[{}{}]}<<[]{}><{}<>>>
{[[{<{[<<<(<<(<><>)<[][]>>{[<>{}]{()<>}}>{{{[]<>}<<>()>}}){(<{<>()}(()<>)>{{{}[]}(()())})]>>({(([<{}()>([]{
[<({(({<<{<<{<<>>{<><>}}((<>{})({}()))>([({}())([][])][{{}{}}<[]<>>])>}((<<<(){}>{{}()}>[{<>()}({}{
<([[[{[(({([{<()<>>}]{(([]<>)[()<>])}){<[{()()}[<><>]]([<><>][{}[]])>{({{}{}}){<{}[]><<><>>}}}}{<({<()<>
<[<((<[[<({{[{<><>}{{}{}}]{<{}{}><(){}>}}{{[[]{}][<>[]]}([<><>][{}()])}}({{{<>()}<<>{}>}([{}]{
{[(<({[{<<{<{{()[]}({}())}{{{}()}<()>}>[<([]{})<<>{}>><{{}()}{{}{}}>]}<{({[][]}{{}[]})}<<{<>{}}[[][]]>>>>
<{[<([(<<([[(<[]<>><{}()>)<({}<>){(){}}>]]<<[(<>{}){{}<>}]([[]{}]({}{}))>>)>{[{[<<[]><{}<>>>(({}{}))
{(<<{{{((<<[({[][]})[([]())(<>{})]][((()<>)([][]))}><([[()[]]]){<<[][]>><<{}[]>[(){}]>}>>[([{[[]<
<{(<<<{{((<{{<{}()>[<>()]}({<>{}}[(){}])}[{({}<>)(()[])}<{(){}}<{}{}>>]>{{[[[]{}]<[][]>]{[{}{}][[]<>]}
{(<(<([{<([<{{<>}<<>{}>}{<()[]>[[]()]}>{<[()()]({}{})>((<>{}))}][{<{[]}[[]()]>{{<>}[[]<>]}}({<<>{}>[[][]]}{<
{((<[<<[{<{[{{{}<>}}<[<>()]([]())>]([<()()><()()>]{[()<>]{[]()}})}<{([[]()]{<>})}(<<[]{}}{[][
[{{{<<<[{[(<[{[]{}}{(){}}}(<()>[[][]])>{{(<><>)<<>()>}<<<>{}>(())>})]{<(<<[]()>({}())><[{}<>]{[]()}>)><
{{[[[({((({{({{}()}<()()>)}{(<{}{}>{()<>})[(()<>)<<><>>]}}<[<[{}[]][()<>]>[[(){}][()()]]>{<{<>[]}[
[<[([{[{[{[{({(){}})}[[<[]{}>{<>()}]]]<[<{<>()}[<><>]>[{<>[]}<()()>]][<{[]{}}({}())>{<[]()>{{}()}}]>
<([{({<(({<{<[[]{}]({}[])>[[<>[]]<<>[]>]}>}{(({(()[])[[]()]})<[[{}<>][<>{}]]<[[]()]([])>>)}))[([[([([][])[<
{<{[([(({<(<{{<>[]}[[]<>]}<(<><>)[[]{}]>>{{{{}()}((){})}))<[{[{}{}]([]{})}{([]())(<>{})}][<(()[])
([[[([<[[{{((<{}{}>(<>()))[({}[]){<>{}}])}}[[(({{}{}}(()[]))<[<>{}]<()<>>>)]<({<()<>>}{{{}<>}{(
(<([{[{<[{[<<<{}()}{{}<>}>({(){}}<[][]>)><{<<>[]>((){})}>](<([()()])[([])({}{})]>[[<{}[]>(<>
([(<{<([{{[[[({}{})[(){}]]]([(()<>)[<>[]]][<{}{}>])][<[<<>[]>[(){}]][[{}[]](<>[])]>({{{}[]>([]{})}[{[][]}[{}
{<[(<[({[<({[({}[])]<{{}<>}[{}()]>}<{{{}{}}[[]]}<(()())[[]<>]>>)>{[{[(<>{})[<>{}]]<({}<>)((){})
{[{[<<<[([[[<{[]{}}[<>()]>(([][])({}[]))]<[[<>()]{<>()}]<[()[]]{[]()}>>]](<{<[()[]]>{({}{})}}(<<[]<>><[]<>>
({[<{{<<({<(<[{}{}]>)[(<{}{}>[[]{}]){<{}()>{<>[]}}]>(({<<><>><<><>>}({[][]}))[[{{}}([]{})]])}[({(<{}[]>{()<>}
({{{{{{[<[<([([]){{}[]}][([]{})])(<([]{})(<>{})><[<>](()())>)>{(([[]()](()())))}]{[[({()[]}{<>(
(({{[<[[[((<{(()<>)[[]{}]}([[]()]{<><>})>))((<{{(){}}(<>())}(<<><>><[]{}>)>(({()()}[{}[]])))<([<<>><()[]>])<
<[[{<({[((<<({{}()}{<><>})<<()[]>[{}()]>>><(<[[][]][()]><[()[]]>)>>){{<((({}<>)(()))(<{}()>)
{<{[([{<{<([{[<>{}][{}{}]}[([]())[[]<>]]]{([<><>]<<>{}})}){[(([][]){<><>}){<<>{}>[()[]]}]<(<()()>{{}})<([]
<{<{<([[[[{[({[][]}<[]{}>){[<>{}]{[]}}]}([[(<>[])][(<><>)<<>{}>]]<([()<>][()()])[<[]()><<>
(<((({{{<([{<[()()]{[]()}>[{{}{}}[()[]]]}]{[(<()<>><[]{}>)<<{}()>[<>[]]}]([[()()]]{(()<>)})}){<{{({}())(
[[[[(([{[(((((<>()){{}{}})(<[]()>({}<>)))<<<()>([]>>((()<>)[<><>])>)([[<()[]><{}()>]{{[][]}([]<
[[{([(<((<[[(([]<>)({}[]))<(())>]]>))>(<([[<{((){})<<>{}>}>{{<()()>[<>[]]}([[]{}]{[][]})}]])[<[<
((([[(<(<[<((<{}()>({}[]))<<<>[]>[()[]]>)([((){})({}())])>][<[(<()()>{{}{}}){<{}<>>{()[]}}]>{[{{{}(
{<(([((<[({<<{<>[]}[[]()]>>([<{}{}]({}())]([()()]<{}<>>))}({[<()()>{[]{}}]((()<>)<()()>)}))([<[[<><>
<[{[<{({([{([({}<>)({}{}>])<([{}{}][()[]])<([]())>>}[{<(<>{})((){})><[<><>](()[])>}<<{<>{}}([])><({}(
({{<<<(([<{<<([]<>)[<>]][{[]{}}{(){}}]>{(({}{}){()<>})}}[({<{}()><(){}>}{([]{})[<>[]]})<[{<>
(<<(<[({[<({<<()<>>({}<>)>})<({<()()><{}{}>}[{(){}}[<><>]]){(({}())<{}<>>)<<(){}>({}<>)>}>>((((<<
[{{{({[{([<<<(<><>)<{}<>>>{{[]()}[()[]]}>{[<<>{}><<>[]>]{[[]{}][()<>]}}>{<<[()<>]><([]{}){<><>}>>[{{{}()}{()[
[[{{(<[[{<<[<[{}{}][{}{}]>>({[(){}]<{}{}>}(((){})[<>[]]))>>}((({<(()<>){{}{}}>{<()[]>[{}[]]}}
(((<{{<{[<<[{{[][]}{[][]}}]{[{[]<>}{()}]}>{{[[[]<>]<(){}>]<[[]()][[]()]>}[<<[]<>>(<>}>{(()<>)<()()>}]}>[
([[{[({{{{[(<<()<>>(<>[])>[(()())<[]{}>])]<{{(<><>)(()<>)}([<><>]<(){}>>}>}[<{({(){}}<()<>>){(<><
{({<{[(<{(({<[[]{}](()())>(((){}))})<(<{<>()}<{}>>{<()()>([]{})})([[<>[]]{[]()}][{<>[]}[{}[]]])>)<{<(<{}()>
{(({[{<{<({{(<[]{}><{}()>)[{{}<>}]}})(<{([()()]{()<>})<{<>()}<[]()>>}<([()[]]([]{}))<{{}{}}<<>[])>>>{({[[]{
<<{{([([[[{<{[<>()]]>{<[[]{}]((){})>{{<>{}}<{}<>>}}}{{{<<>{}>({}<>)}[<(){}><{}()>]}([<{}<>>(<>[])
[{<[<{{{{<({{<{}()>{{}<>}}})[(<{<>[]}{<>()}>[(<>())<()>])<<{()[]}<<>()>>{([]<>>}>]><({[[{}[]]
[{{<(<({(<{[<[<>[]]{()<>}><(<>[]){{}}>]{<<<>{}>[<>{}]><[[]<>]{<>()})}}>)}[[({{<[()()][(){}]>((()[])(
[[[[[[[[<<([<[()[]](<><>)><(<>[])<<>{}>>])><{[<<<>{}><{}()>>({[][]}<[]{}>)]([{<>()}{{}[]}]{{{}{}}{[]()}}
<(((<(([<{(<[[[][]]{()[]}]<(<>{}){[]{}}>>)}>]))[({[<<{(<()[]>([]<>))[<()[]>{()[]}]}<([<><>](<>[]))((<>()
<{{{<{{{[[{([({}{})<[][]>]<[<>{}]{<>()}>)}<({{[]<>}([]())}<<<>[]><<>{}>>){(<[]><(){}>)>>]{{(<([]<>)([](
<[<{[<{{([(({(<>())}{[<>[]](()<>)})([([][]){<>()}][<[]<>>{[]{}}]))])}([{{(<{<>()}[()<>])((
{{[({[[{([<<{<()()>[()[]]}{<<>{}>{{}()}}>>]<{<[<{}()>{[][]}][{[][]}[<><>>]>}>)}]][<((({({([]{})
[(<[([<{[[{{<{{}[]}({}())>([{}[]]{[]})}([([]())<<><>>]<(())<<>>>)}[<[(<>{}){{}<>}]{{()<>}[<>{
{{[{[<([{<[((<(){}><()[]>){[{}]({}<>)})(([()[]]<<>[]>)<[(){}]>]]{({[<>()][<>[]]}({[]{}}{{}[]}))[[[(){}][[]{}
{[<<<[<[{[<((<[]{}>)<([]<>){{}<>}>)<{([]())({}{})}>>](<([(<><>)[[]<>]]<{<><>}{()[]}>)>[(<<<>()><()[]>>
<[<((<<(<{(({<{}<>>{{}<>}}{<[]()>{<>{}}})){<[(<><>)<()<>>]>[{{<>{}}<(){}>}<{<><>}({}())>]}}>)>[[{<[[
[([((<<([<{<[[()()]][[{}[]]]>{{(<>{})<<>()>][{[]()}]}}>])<([{<({{}()}<<>{}>)><[[()]<()[]>][[()<>](<>
{{((<[(({<[<[<[]<>>[(){}]](<[]<>><{}[]>)>][({{[]()}{<>[]}}{(()[]){<><>}})<[<{}[]>]>]>}<{[{{{<>[]}{{}()}}{{<>
({{({{{{{{[<[({}())[<><>]]>[(<<>[]>((){}))]]{{(<<>()>[{}[]])[{<>{}}<{}[]>]}{({{}()})}}}[[({{{}<>}<()()>}({()
[<<[((({<<<(({(){}}({}{})){<<>{}>{{}{}}}]((<[][]>{<>[]}))>>{[[([()()])<[()[]]{[]<>}>][(<<>()>({}[]))<({}[]
{(<<(<{({<<[[(<><>){(){}}][([]())([][])]](<<<>()>[[][]]>[({}{})[()<>]])>((<[[]<>]<{}()>>){<([][])[[]<>
<([{<[({{{[<{{()<>}(<><>)}{<{}()>[{}<>]}>{{[{}{}]<<>>}<[<>()]<<>[]>>}]{<<([][]){[]()}>[<{}()><()()>]>[
(<<[[<(<{[<(([()<>]<()<>>){[{}[]][{}()]})<[[<><>]<[]>]<[[]()]{[]{}}>>>}<[[[<{}{}>(<>())]]{({<>{}}([
[(([<(<(([(<<<{}{}>[[]()]>[<[][]>{[]}]><<<<>()>[[]{}]>([[][]]{<>()})>)(({<<><>>[()()]}))]))>((({((
<<[[<{{({[{({(<><>)(()<>)}[[<><>]])[({<><>}([][]))[[[]()]([]{})]]}]<{({(<>{})[(){}]}({()<>}))([{(
({[<<<<[<(<[<[[]<>]([]<>)>{<(){}><[]()>}]{[(()<>)(()())][{(){}}[<>]]}>)>][(([({([][])<[][]>][{(){}}({})])<<{[
<<({[(<{<(<({<[]<>>{()()}})<{<()[]>[[]()]}(((){}){<>})>>){[[<[{}()]>][{[(){}]({}[])}<[<>[]](<><>)>]]({(<<
<[([[(<<({(<<({}())]>{[[[]<>][[]()]]})<<{([]())[[]<>]}<(())<{}<>>>>{(([]{}){<>{}}){[{}()][(){}]}}>})><
{(<{{[{<({[[[[{}()]({}[])]({()()}{{}[]})]{<[(){}]>([{}()]{{}{}})}](<{<{}[]>[{}[]]}((()[]){<>{}})>([{[]{
<[(<({<(<[<{<([]<>)<<><>>>([<><>])}>]{([{(<><>)(<>())}[{()()}{(){}}]]){{{([]{}}{{}[]}}<{<>[]}[[
<[<[[{([<({([<[]()><[]<>>])[([[]<>]{{}})]}<<<<[]<>>(()[])>({()()}<<>[]>)>[<{[]<>}>({()()}(()<>))]>)({(
{{<[(<{{{<[<(([]<>}[<>{}])><<<<>{}>([]())>[([][])]>]>}}}>)]>(({(<{<<<(({()}<[][]>)[<[]>{<><>}
({{[<[({[[{({{{}[])}<[<>[]][<><>]>)<{{[]{}}{{}[]}}>}]<((<(<>()){()}>)(<[{}()]{<><>}>(<()()>{()[]})'''.split('\n')