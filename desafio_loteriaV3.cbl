      $set sourceformat"free"

      *>Divisão de identificação do programa
       identification division.
       program-id. "desafio_loteria".
       author. "Daiana Weiss".
       installation. "PC".
       date-written. 21/07/2020.
       date-compiled. 21/07/2020.



      *>Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>-----Declaração dos recursos externos
       input-output section.
       file-control.
       i-o-control.

      *>Declaração de variáveis
       data division.

      *>----Variaveis de arquivos
       file section.


      *>----Variaveis de trabalho
       working-storage section.
       01 ws-mensagens.
           05 ws-mensagem.
               10 ws-msn                          pic zzzzzzzz9(10).


       01 ws-numeros-usu occurs 10.
           05 ws-num-usu                           pic 9(02) value zero.

       01 ws-numeros-sort.
           05 ws-num-sort1                         pic 9(02) value 61.
           05 filler                               pic X(02) value spaces.
           05 ws-num-sort2                         pic 9(02) value 61.
           05 filler                               pic X(02) value spaces.
           05 ws-num-sort3                         pic 9(02) value 61.
           05 filler                               pic X(02) value spaces.
           05 ws-num-sort4                         pic 9(02) value 61.
           05 filler                               pic X(02) value spaces.
           05 ws-num-sort5                         pic 9(02) value 61.
           05 filler                               pic X(02) value spaces.
           05 ws-num-sort6                         pic 9(02) value 61.


       01 ws-num-sort-rel.
           05 ws-num-sort1-rel                     pic 9(02) value 61.
           05 filler                               pic X(02) value spaces.
           05 ws-num-sort2-rel                     pic 9(02) value 61.
           05 filler                               pic X(02) value spaces.
           05 ws-num-sort3-rel                     pic 9(02) value 61.
           05 filler                               pic X(02) value spaces.
           05 ws-num-sort4-rel                     pic 9(02) value 61.
           05 filler                               pic X(02) value spaces.
           05 ws-num-sort5-rel                     pic 9(02) value 61.
           05 filler                               pic X(02) value spaces.
           05 ws-num-sort6-rel                     pic 9(02) value 61.


       01 ws-sorteio.
           05 ws-semente                           pic 9(08).
           05 ws-num_random                        pic 9(02)V99.
           05 ws-controle-sort                     pic 9(01).

       01 ws-controle-acertou1                     pic 9(01).
           88 ws-acertou1                          value 1.
           88 ws-nao-acertou1                       value 0.

       01 ws-controle-acertou2                     pic 9(01).
           88 ws-acertou2                          value 1.
           88 ws-nao-acertou2                       value 0.

       01 ws-controle-acertou3                     pic 9(01).
           88 ws-acertou3                          value 1.
           88 ws-nao-acertou3                       value 0.

       01 ws-controle-acertou4                     pic 9(01).
           88 ws-acertou4                          value 1.
           88 ws-nao-acertou4                       value 0.

       01 ws-controle-acertou5                     pic 9(01).
           88 ws-acertou5                          value 1.
           88 ws-nao-acertou5                       value 0.

       01 ws-controle-acertou6                     pic 9(01).
           88 ws-acertou6                          value 1.
           88 ws-nao-acertou6                       value 0.


       01 ws-vars-trabalho.
           05 i                                    pic 9(02).
           05 ws-num-tent                          pic 9(10).
           05 ws-verificacao                       pic X(01).
               88 ws-valido                        value "V".
               88 ws-invalido                      value "I".

       77 ws-qtd-num                               pic 9(02).

       77 ws-hora-inicial                          pic 9(06).
       77 ws-hora-final                            pic 9(06).
       77 ws-tempo-gasto                           pic 9(06).


       77 ws-aux                                      pic 9(02).

       77 ws-controle                                 pic x(10).
      *>----Variaveis para comunicação entre programas
       linkage section.


      *>----Declaração de tela
       screen section.


      *>Declaração do corpo do programa
       procedure division.

           perform inicializa.
           perform processamento.
           perform finaliza.

      *>------------------------------------------------------------------------
      *>  Procedimentos de inicialização
      *>------------------------------------------------------------------------
       inicializa section.
           move zeros to ws-num-tent
           set ws-nao-acertou1 to true
           set ws-nao-acertou2 to true
           set ws-nao-acertou3 to true
           set ws-nao-acertou4 to true
           set ws-nao-acertou5 to true
           set ws-nao-acertou6 to true

           move 1 to i
           .
       inicializa-exit.
           exit.
      *>------------------------------------------------------------------------
      *>  Processamento principal
      *>------------------------------------------------------------------------
       processamento section.
           display "Quantos numeros vc deseja cadastrar? (6 - 10)"
           accept ws-qtd-num

      *>   garantindo que seja valido
           if ws-qtd-num < 6 or ws-qtd-num > 10 then
               display "Entrada invalida. O numero deve estar entre 1 e 60."
               perform finaliza
           end-if

           perform recebe-numeros


           .
       processamento-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Recebendo os numeros do usuario
      *>------------------------------------------------------------------------

       recebe-numeros section.

           move 00 to i
           set ws-valido to true

           perform varying i from 1 by 1 until i > ws-qtd-num or ws-invalido
               display "Numero: "
               accept ws-num-usu(i)

               perform verifica-numero
           end-perform

      *>   vai verificar se os numeros sao diferentes
           perform verifica-numeros-dif


           accept ws-hora-inicial from time

      *>   se o numero for valido, entao ele vai para o sorteio e o restante do programa
           if ws-valido then
      *>       vai fazer ate que o sorteio acerte o numero
               perform until ws-acertou1 and ws-acertou2 and ws-acertou3
               and ws-acertou4 and ws-acertou5 and ws-acertou6
                   add 1 to ws-num-tent
                   move ws-num-tent to ws-msn

                   perform sorteia-num
                   perform verifica-acertou-sorteio
               end-perform
           end-if

           if ws-acertou1 and ws-acertou2 and ws-acertou3
           and ws-acertou4 and ws-acertou5 and ws-acertou6 then
      *>       calculo de tempo gasto ate o acerto
               accept ws-hora-final from time
               compute ws-tempo-gasto = ws-hora-final - ws-hora-inicial

               display "Quantidade de Tentativas : " ws-mensagem
               display "Tempo Gasto              : " ws-tempo-gasto
           end-if

           .
       recebe-numeros-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Sorteando os numeros da loteria
      *>------------------------------------------------------------------------
       sorteia-num section.

      *>   inicializando variaveis
           move 61 to ws-num-sort1
           move 61 to ws-num-sort2
           move 61 to ws-num-sort3
           move 61 to ws-num-sort4
           move 61 to ws-num-sort5
           move 61 to ws-num-sort6


      *>   se ainda nao acertou o 1, entao sorteia de novo
               accept ws-semente from time
               compute ws-num_random = function random(ws-semente)
               multiply ws-num_random by 60 giving ws-num-sort1

      *>   se ainda nao acertou o 2, entao sorteia de novo
               perform until ws-num-sort2 <> ws-num-sort1 and ws-num-sort2 <> 61
      *>           sorteando o segundo numero, garantindo que nao seja um numero ja sorteado
                   accept ws-semente from time
                   compute ws-num_random = function random(ws-semente)
                 multiply ws-num_random by 60 giving ws-num-sort2
              end-perform

      *>   se ainda nao acertou o 3, entao sorteia de novo
              perform until ws-num-sort3 <> ws-num-sort2 and ws-num-sort3 <> ws-num-sort1 and ws-num-sort3 <> 61
      *>           sorteando o terceiro numero, garantindo que nao seja um numero ja sorteado
                   accept ws-semente from time
                   compute ws-num_random = function random(ws-semente)
                   multiply ws-num_random by 60 giving ws-num-sort3
               end-perform

      *>   se ainda nao acertou o 4, entao sorteia de novo
               perform until ws-num-sort4 <> ws-num-sort2 and ws-num-sort4 <> ws-num-sort1
               and ws-num-sort4 <> ws-num-sort3 and ws-num-sort4 <> 61
      *>           sorteando o quarto numero, garantindo que nao seja um numero ja sorteado
                   accept ws-semente from time
                   compute ws-num_random = function random(ws-semente)
                   multiply ws-num_random by 60 giving ws-num-sort4
               end-perform

      *>   se ainda nao acertou o 5, entao sorteia de novo
               perform until ws-num-sort5 <> ws-num-sort2 and ws-num-sort5 <> ws-num-sort1
               and ws-num-sort5 <> ws-num-sort3 and ws-num-sort5 <> ws-num-sort4 and ws-num-sort5 <> 61
      *>           sorteando o quinto numero, garantindo que nao seja um numero ja sorteado
                   accept ws-semente from time
                   compute ws-num_random = function random(ws-semente)
                   multiply ws-num_random by 60 giving ws-num-sort5
               end-perform

      *>   se ainda nao acertou o 6, entao sorteia de novo
               perform until ws-num-sort6 <> ws-num-sort2 and ws-num-sort6 <> ws-num-sort1
               and ws-num-sort6 <> ws-num-sort3 and ws-num-sort6 <> ws-num-sort4
               and ws-num-sort6 <> ws-num-sort5 and ws-num-sort6 <> 61
      *>           sorteando o sexto numero, garantindo que nao seja um numero ja sorteado
                   accept ws-semente from time
                   compute ws-num_random = function random(ws-semente)
                   multiply ws-num_random by 60 giving ws-num-sort6
               end-perform

           perform ordenar-num-sort
           move ws-numeros-sort to ws-num-sort-rel
           display "Numeros sorteados: " ws-num-sort-rel
           accept ws-aux

           .
       sorteia-num-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Verificando se os numeros do usuario sao validos (entre 1 e 60)
      *>------------------------------------------------------------------------
       verifica-numero section.
           if i <> 00 then
      *>           garantindo que o numero inserido seja valido (entra 1 e 60)
               if ws-num-usu(i) > 60 or < 1 then
                   display "O numero apostado deve estar entre 1 e 60."
                   perform finaliza
                   set ws-invalido to true
               end-if
           end-if

           .
       verifica-numero-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Verificando se os numeros do usuario nao estao repetidos
      *>------------------------------------------------------------------------

       verifica-numeros-dif section.
      *>   se o numero estiver entre 1 e 60, entao garante que nao seja repetido

           if ws-valido then
               if ws-num-usu(1) = ws-num-usu(2) or ws-num-usu(1) = ws-num-usu(3)
               or ws-num-usu(1) = ws-num-usu(4) or ws-num-usu(1) = ws-num-usu(5)
               or ws-num-usu(1) = ws-num-usu(6) then
                   set ws-invalido to true
                   display "O numero apostado nao pode se repetir."
                   perform finaliza
               end-if

               if ws-num-usu(2) = ws-num-usu(1) or ws-num-usu(2) = ws-num-usu(3)
               or ws-num-usu(2) = ws-num-usu(4) or ws-num-usu(2) = ws-num-usu(5)
               or ws-num-usu(2) = ws-num-usu(6) then
                   set ws-invalido to true
                   display "O numero apostado nao pode se repetir."
                   perform finaliza
               end-if

               if ws-num-usu(3) = ws-num-usu(1) or ws-num-usu(3) = ws-num-usu(2)
               or ws-num-usu(3) = ws-num-usu(4) or ws-num-usu(3) = ws-num-usu(5)
               or ws-num-usu(3) = ws-num-usu(6) then
                   set ws-invalido to true
                   display "O numero apostado nao pode se repetir."
                   perform finaliza
               end-if

               if ws-num-usu(4) = ws-num-usu(1) or ws-num-usu(4) = ws-num-usu(2)
               or ws-num-usu(4) = ws-num-usu(3) or ws-num-usu(4) = ws-num-usu(5)
               or ws-num-usu(4) = ws-num-usu(6) then
                   set ws-invalido to true
                   display "O numero apostado nao pode se repetir."
                   perform finaliza
               end-if

               if ws-num-usu(5) = ws-num-usu(1) or ws-num-usu(5) = ws-num-usu(2)
               or ws-num-usu(5) = ws-num-usu(3) or ws-num-usu(5) = ws-num-usu(4)
               or ws-num-usu(5) = ws-num-usu(6) then
                   set ws-invalido to true
                   display "O numero apostado nao pode se repetir."
                   perform finaliza
               end-if

               if ws-num-usu(6) = ws-num-usu(1) or ws-num-usu(6) = ws-num-usu(2)
               or ws-num-usu(6) = ws-num-usu(3) or ws-num-usu(6) = ws-num-usu(4)
               or ws-num-usu(6) = ws-num-usu(5) then
                   set ws-invalido to true
                   display "O numero apostado nao pode se repetir."
                   perform finaliza
               end-if

               if ws-qtd-num = 7 then
                   if ws-num-usu(7) = ws-num-usu(1) or ws-num-usu(7) = ws-num-usu(2)
                   or ws-num-usu(7) = ws-num-usu(3) or ws-num-usu(7) = ws-num-usu(4)
                   or ws-num-usu(7) = ws-num-usu(5) or ws-num-usu(7) = ws-num-usu(6) then
                       set ws-invalido to true
                       display "O numero apostado nao pode se repetir."
                       perform finaliza
                   end-if
               end-if

               if ws-qtd-num = 8 then
                   if ws-num-usu(7) = ws-num-usu(1) or ws-num-usu(7) = ws-num-usu(2)
                   or ws-num-usu(7) = ws-num-usu(3) or ws-num-usu(7) = ws-num-usu(4)
                   or ws-num-usu(7) = ws-num-usu(5) or ws-num-usu(7) = ws-num-usu(6) then
                       set ws-invalido to true
                       display "O numero apostado nao pode se repetir."
                       perform finaliza
                   end-if

                   if ws-num-usu(8) = ws-num-usu(1) or ws-num-usu(8) = ws-num-usu(2)
                   or ws-num-usu(8) = ws-num-usu(3) or ws-num-usu(8) = ws-num-usu(4)
                   or ws-num-usu(8) = ws-num-usu(5) or ws-num-usu(8) = ws-num-usu(6)
                   or ws-num-usu(8) = ws-num-usu(7) then
                       set ws-invalido to true
                       display "O numero apostado nao pode se repetir."
                       perform finaliza
                   end-if
               end-if

               if ws-qtd-num = 9 then
                   if ws-num-usu(7) = ws-num-usu(1) or ws-num-usu(7) = ws-num-usu(2)
                   or ws-num-usu(7) = ws-num-usu(3) or ws-num-usu(7) = ws-num-usu(4)
                   or ws-num-usu(7) = ws-num-usu(5) or ws-num-usu(7) = ws-num-usu(6) then
                       set ws-invalido to true
                       display "O numero apostado nao pode se repetir."
                       perform finaliza
                   end-if

                   if ws-num-usu(8) = ws-num-usu(1) or ws-num-usu(8) = ws-num-usu(2)
                   or ws-num-usu(8) = ws-num-usu(3) or ws-num-usu(8) = ws-num-usu(4)
                   or ws-num-usu(8) = ws-num-usu(5) or ws-num-usu(8) = ws-num-usu(6)
                   or ws-num-usu(8) = ws-num-usu(7) then
                       set ws-invalido to true
                       display "O numero apostado nao pode se repetir."
                       perform finaliza
                   end-if

                   if ws-num-usu(9) = ws-num-usu(1) or ws-num-usu(9) = ws-num-usu(2)
                   or ws-num-usu(9) = ws-num-usu(3) or ws-num-usu(9) = ws-num-usu(4)
                   or ws-num-usu(9) = ws-num-usu(5) or ws-num-usu(9) = ws-num-usu(6)
                   or ws-num-usu(9) = ws-num-usu(7) or ws-num-usu(9) = ws-num-usu(8) then
                       set ws-invalido to true
                       display "O numero apostado nao pode se repetir."
                       perform finaliza
                   end-if

               end-if

               if ws-qtd-num = 10 then

                   if ws-num-usu(7) = ws-num-usu(1) or ws-num-usu(7) = ws-num-usu(2)
                   or ws-num-usu(7) = ws-num-usu(3) or ws-num-usu(7) = ws-num-usu(4)
                   or ws-num-usu(7) = ws-num-usu(5) or ws-num-usu(7) = ws-num-usu(6) then
                       set ws-invalido to true
                       display "O numero apostado nao pode se repetir."
                       perform finaliza
                   end-if

                   if ws-num-usu(8) = ws-num-usu(1) or ws-num-usu(8) = ws-num-usu(2)
                   or ws-num-usu(8) = ws-num-usu(3) or ws-num-usu(8) = ws-num-usu(4)
                   or ws-num-usu(8) = ws-num-usu(5) or ws-num-usu(8) = ws-num-usu(6)
                   or ws-num-usu(8) = ws-num-usu(7) then
                       set ws-invalido to true
                       display "O numero apostado nao pode se repetir."
                       perform finaliza
                   end-if

                   if ws-num-usu(9) = ws-num-usu(1) or ws-num-usu(9) = ws-num-usu(2)
                   or ws-num-usu(9) = ws-num-usu(3) or ws-num-usu(9) = ws-num-usu(4)
                   or ws-num-usu(9) = ws-num-usu(5) or ws-num-usu(9) = ws-num-usu(6)
                   or ws-num-usu(9) = ws-num-usu(7) or ws-num-usu(9) = ws-num-usu(8) then
                       set ws-invalido to true
                       display "O numero apostado nao pode se repetir."
                       perform finaliza
                   end-if

                   if ws-num-usu(10) = ws-num-usu(1) or ws-num-usu(10) = ws-num-usu(2)
                   or ws-num-usu(10) = ws-num-usu(3) or ws-num-usu(10) = ws-num-usu(4)
                   or ws-num-usu(10) = ws-num-usu(5) or ws-num-usu(10) = ws-num-usu(6)
                   or ws-num-usu(10) = ws-num-usu(7) or ws-num-usu(10) = ws-num-usu(8)
                   or ws-num-usu(10) = ws-num-usu(9) then
                       set ws-invalido to true
                       display "O numero apostado nao pode se repetir."
                       perform finaliza
                   end-if

               end-if
           end-if
           .
       verifica-numeros-dif-exit.
           exit.



      *>------------------------------------------------------------------------
      *>  Ordenando os numeros que o usuario apostou
      *>------------------------------------------------------------------------
       ordenar-num-sort section.
           move "trocou" to ws-controle
           perform until ws-controle <> "trocou"
               move     1        to    i
               move  "N_trocou"  to ws-controle
               perform until i = 6
                   if ws-num-sort1 > ws-num-sort2 then
                       move  ws-num-sort2      to   ws-aux
                       move    ws-num-sort1    to   ws-num-sort2
                       move      ws-aux        to   ws-num-sort1
                       move    "trocou"        to   ws-controle
                   end-if

                   if ws-num-sort2 > ws-num-sort3 then
                       move  ws-num-sort3      to   ws-aux
                       move    ws-num-sort2    to   ws-num-sort3
                       move      ws-aux        to   ws-num-sort2
                       move    "trocou"        to   ws-controle
                   end-if

                   if ws-num-sort3 > ws-num-sort4 then
                       move  ws-num-sort4      to   ws-aux
                       move    ws-num-sort3    to   ws-num-sort4
                       move      ws-aux        to   ws-num-sort3
                       move    "trocou"        to   ws-controle
                   end-if

                   if ws-num-sort4 > ws-num-sort5 then
                       move  ws-num-sort5      to   ws-aux
                       move    ws-num-sort4    to   ws-num-sort5
                       move      ws-aux        to   ws-num-sort4
                       move    "trocou"        to   ws-controle
                   end-if

                   if ws-num-sort5 > ws-num-sort6 then
                       move  ws-num-sort6      to   ws-aux
                       move    ws-num-sort5    to   ws-num-sort6
                       move      ws-aux        to   ws-num-sort5
                       move    "trocou"        to   ws-controle
                   end-if

                   add 1 to i
               end-perform
           end-perform
           .
       ordenar-num-sort-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Verificando se os numeros sorteados sao iguais ao da aposta
      *>------------------------------------------------------------------------
       verifica-acertou-sorteio section.

           set ws-nao-acertou1 to true
           set ws-nao-acertou2 to true
           set ws-nao-acertou3 to true
           set ws-nao-acertou4 to true
           set ws-nao-acertou5 to true
           set ws-nao-acertou6 to true


           perform varying i from 1 by 1 until i > ws-qtd-num or (ws-acertou1 and ws-acertou2 and ws-acertou3
           and ws-acertou4 and ws-acertou5 and ws-acertou6)

               if ws-nao-acertou1 then
                   if ws-num-sort1 = ws-num-usu(i)  *> todos os 7
                   then
                       set ws-acertou1 to true
                   else
                       set ws-nao-acertou1 to true
                   end-if
               end-if

               if ws-nao-acertou2 then
                   if ws-num-sort2 = ws-num-usu(i) then
                       set ws-acertou2 to true
                   else
                       set ws-nao-acertou2 to true
                   end-if
               end-if

               if ws-nao-acertou3 then
                   if ws-num-sort3 = ws-num-usu(i) then
                       set ws-acertou3 to true
                   else
                       set ws-nao-acertou3 to true
                   end-if
               end-if

               if ws-nao-acertou4 then
                   if ws-num-sort4 = ws-num-usu(i) then
                       set ws-acertou4 to true
                   else
                       set ws-nao-acertou4 to true
                   end-if
               end-if

               if ws-nao-acertou5 then
                   if ws-num-sort5 = ws-num-usu(i) then
                       set ws-acertou5 to true
                   else
                       set ws-nao-acertou5 to true
                   end-if
               end-if

               if ws-nao-acertou6 then
                   if ws-num-sort6 = ws-num-usu(i) then
                       set ws-acertou6 to true
                   else
                       set ws-nao-acertou6 to true
                   end-if
               end-if

           end-perform

           .
       verifica-acertou-sorteio-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Finalização
      *>------------------------------------------------------------------------
       finaliza section.
           Stop run
           .
       finaliza-exit.
           exit.


