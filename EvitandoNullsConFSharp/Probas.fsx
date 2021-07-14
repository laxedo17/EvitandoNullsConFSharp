// Botaremos unha ollada a unha Discriminated Union específica construída
// en F # e deseñada para resolver un único problema: nada. Ou, máis seriamente, o manexo de valores nulos null, o chamado problema do billon de dolares.

// Todos os tipos F # (tuplas, rexistros, e discriminated unions) compórtanse do mesmo xeito, xa que todos son obrigatorios por defecto. É ilegal asignar null a calquera symbol declarado en F#. (Isto tamén aplícase a clases e interfaces definidas en F#)

//O type option
// F # ten un tipo Option (tamén coñecido como Maybe nalgunhas linguaxes). Pode considerarse como unha especie de nullable, excepto que é máis flexible (pode funcionar non só en tipos F #, senón tamén en clases e structs) e ten soporte na linguaxe para facilitar o traballo con máis seguridade en ambos os casos de "ten un valor" e "sen valor" 
let unNumero : int = 10
let quizaisUnNumero : int option = Some 10 //Creadndo un numero opcional

let calcularPremiunEurosAnuales puntuacion = 
    match puntuacion with
    | Some 0 -> 250 //Manexando unha puntuacion de segurido de (Some 0)
    | Some puntuacion when puntuacion < 0 -> 400
    | Some puntuacion when puntuacion > 0-> 150
    | None -> //Manexando o caso de non atoparse a puntuacion de seguridade
        printfn "Non se engadiu puntuacion algunha! Usando un premium temporal"
        300
    calcularPremiunEurosAnuales (Some 250) //Calculando un premium con un wrapper da puntuacion de (Some 250) e logo None
    calcularPremiunEurosAnuales None

// A Option<T> é unha unión discriminada simple con dous casos: Some (valor) ou None. Igual que con outras discriminated unions, fas pattern matching para tratar todos os casos e a diferenza das comprobacións de null reference das clases, aquí debes xestionar explícitamente os dous casos (valor e sen valor) antes do momento da compilación; non se pode saltar a verificación de null.
// Tamén ten en conta que cando chamas a esta función xa non podes pasar o valor 250 por exemplo. Primeiro debes envolvelo como Some 250 (igual que axustamos o número de pines nun disco MMC). Isto é lixeiramente diferente de traballar con nullables en C #, onde
// o compilador envolverá en silencio un enteiro nun int anulable. F # é un pouco máis estrito aquí e debes envolver o número de xeito explícito. 

// 1 Crea un tipo de rexistro que coincida coa estrutura do cliente.
// 2 Para o tipo de campo opcional, usa unha opción (opción int ou opción <int>).
// 3 Crea unha lista que conteña os dous clientes usando [a; b] sintaxe.
// 4 Cambia a función do listado anterior para incorporar un obxecto cliente completo e fai match co campo PuntuacionDeSeguridade do mesmo. 
type Conductor = { Nome : string; PuntuacionDeSeguridade : int option; AnoTestPasado : int}

let conductores = 
    [ { Nome = "Laurie L'anui"; PuntuacionDeSeguridade = Some 550; AnoTestPasado = 1980 }
      { Nome = "Alba Sinner"; PuntuacionDeSeguridade = None; AnoTestPasado = 1980 } ]

let calcularPremiumPorCliente cliente =
    match cliente.PuntuacionDeSeguridade with  
    | Some 0 -> 250
    | Some puntuacion when puntuacion < 0 -> 400
    | Some puntuacion when puntuacion > 0 -> 150
    | None ->
        printfn "Non se proporcionou unha puntuacion. Usando premium temporal"
        300

//Mapping
// Option.map permíteche asignar un valor opcional dun tipo de opction a outro por
// medios dunha función de mapeo:
// mapping :( 'T ->' U) -> option: 'Option T ->' Option U
// Ten un propósito similar a List.map:
// mapping :( 'T ->' U) -> list: 'List T -> 'List U
// Noutras palabras, é unha función de orde superior que toma un valor opcional e unha función de mapeo para actuar sobre el, pero chama ao mapeo só se o valor é Some. Se o valor é None non fai nada. Isto é similar a como List.map chama ao mapeador só se hai polo menos un
// elemento da lista. Se é unha lista baleira, non pasa nada. E como con List.map, a función de mapeo non ten que saber sobre (neste caso) as options; tómase o acto de comprobar
// coida diso.
// Imos ponher un exemplo. Imaxina que un colega escribiu unha función, describir,
// que describe a puntuación de seguridade dun condutor (por exemplo, seguro ou de alto risco). Non está deseñado para traballar con puntuacións opcionais, pero queres executalo contra puntuacións de seguridade opcionales do teu ficheiro JSON. Podes empregar Pattern Matching ou podes usar Option.map 
let describir =
    match cliente.PuntuacionDeSeguridade with
    | Some puntuacion -> Some (describir puntuacion) //un match estandar sobre unha option
    | None -> None
let describirDous =
    cliente.PuntuacionDeSeguridade
    |> Option.map(fun puntuacion -> describir puntuacion) //Usando Option.map para actuar no caso Some
//Shorthand para evitar ter que proporcionar argumentos explicitamente a funcion describir en Option.map
let clave = cliente.PuntuacionDeSeguridade |> Option.map describir
//Crea unha nova funcion que executa con seguridade a funcion describr sobre valores opcionales
let describirOpcional = Option.map describir
//As 3 expresions fan o mesmo: so lanzan a funcion describir se PuntuacionDeSeguridade ten un valor Some, senon non fan nada.

//Binding
// Option.bind é o mesmo que Option.map, excepto que funciona con funcións de asignación que elas mesmas devolven options:
// binder :( 'T ->' Option U) -> option: 'Option T -> 'Option U
// Bind é máis ou menos o equivalente a List.collect (ou SelectMany en LINQ). Pode aplanar unha Option<Option<string>> a Option<string>, do mesmo xeito que collect pode aplanar unha List <Lista<string>> a List<string>. Isto é útil se encadeas varias funcións xuntas, cada unha das cales devolve unha option 

//Encadenando funcions que devolven unha option con Option.bind
let intentaAtoparCiente idCliente = if idCliente = 10 then Some conductores.[0] else None
let getPuntuacionDeSeguridade cliente = cliente.PuntuacionDeSeguridade //Duas funcions que ambas devolven un valor opcional
let puntuacion = intentaAtoparCiente 10 |> Option.bind getPuntuacionDeSeguridade //Encadeando ambas funcions xuntas

//Filtering
// Tamén podes filtrar unha option usando Option.filter. Noutras palabras, tes un predicado
// sobre un valor opcional. Se o valor é Some, executa o predicado. Se pasa, mantén o
// valor opcional; se non, devolve None.
// predicate:( 'T -> bool) -> option:' option T -> 'option T. 
let test1 = Some 5 |> Option.filter(fun x -> x > 5) //test1 e igual a None 
let test2 = Some 5 |> Option.filter(fun x -> x = 5) //test2 e igual a (Some 5)

// Imaxinemos que temos unha base de datos de clientes con identificacións asociadas e unha lista de identificacións de clientes. Queremos cargar os nomes deses clientes da base de datos, pero non estás seguro de se todos os teus ID de cliente son válidos. Como se poden devolver facilmente só os clientes que existen? Sigue estes pasos:
// 1 Crea unha función tryLerCliente que conteña un ID de cliente. Se a identificación está entre 2 e 7, devolve unha cadea opcional "Cliente <id>" (por exemplo, "Cliente 4"). En caso contrario, devolve None.
// 2 Crea unha lista de identificacións de cliente de 0 a 10.
// 3 Envía os ID de clientes a través de List.choose, usando o tryLerCliente como
// función de orde superior.
// 4 Observa que tes unha nova lista de cadeas, pero só para clientes existentes.
let tryLerClient id =
    if id >= 2 && id <= 7 then Some (sprintf "Cliente %d" id)
    else None
[ 1 .. 10 ]
|> List.choose tryLerCliente 