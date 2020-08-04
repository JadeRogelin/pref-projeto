      $set sourceformat"free"
      *>----Divisão de identificação do programa
       identification division.
       program-id. "P05SISC20".
       author. "Jade Rogelin".
       installation. "PC".
       date-written. 03/08/2020.
       date-compiled. 03/08/2020.

      *>----Divisão para configuração do ambiente
       environment division.
       configuration section.
       special-names.
       decimal-point is comma.

      *>----Declaração dos recursos externos
       input-output section.
       file-control.

           select arq-preferencias assign to "arq-preferencias.dat"
           organization is indexed
           access mode is dynamic
           lock mode is manual with lock on multiple records
           record key is fl-user-id
           file status is ws-fs-arq-preferencias.

       i-o-control.


      *>----Declaração de variáveis
       data division.

      *>----Variáveis de arquivos
       file section.
       fd arq-preferencias.
       01 fl-preferencias.
           05 fl-user-id                           pic X(08).
           05 fl-idioma                            pic X(02).
           05 fl-versao                            pic X(05).
           05 fl-cifra-vigenere                    pic X(25).
           05 fl-modo                              pic X(01). *> ‘P’-rova; ‘S’-imulado


      *>----Variáveis de trabalho
       working-storage section.

       77 ws-fs-arq-preferencias                   pic X(02).

       01 ws-preferencias.
           05 ws-user-id                           pic X(08).
           05 ws-idioma                            pic X(02).
           05 ws-versao                            pic X(05).
           05 ws-cifra-vigenere                    pic X(25).
           05 ws-modo                              pic X(01). *> ‘P’-rova; ‘S’-imulado

       77 ws-msn                                   pic X(50).


       01 ws-msn-erro.
          05 ws-msn-erro-ofsset                    pic 9(04).
          05 filler                                pic X(01) value "-".
          05 ws-msn-erro-cod                       pic X(02).
          05 filler                                pic X(01) value space.
          05 ws-msn-erro-text                      pic X(42).

       01 ws-tela-pref-adm.
          05 ws-cadastrar-prova                    pic X(02).
          05 ws-cadastrar-simulado                 pic X(02).

      01 ws-uso-telas.
          05 ws-sair                               pic X(02).

       01 ws-tela-pref-usu.
          05 ws-prova                              pic X(02).
          05 ws-simulado                           pic X(02).


      *>----Variáveis para comunicação entre programas
       linkage section.


      *>----Declaração de tela
       screen section.

       01  tela-pref-adm.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "    ///////////////////////////////////////////////////////////////////////////  ".
           05 line 02 col 01 value "                                                                     [ ]Sair     ".
           05 line 03 col 01 value "                             Preferencias Administrador                          ".
           05 line 04 col 01 value "      MENU                                                                       ".
           05 line 05 col 01 value "        [ ] CP - Cadastrar Prova                                                 ".
           05 line 06 col 01 value "        [ ] CS - Cadastrar Simulado                                              ".
           05 line 07 col 01 value "                                                                                 ".
           05 line 08 col 01 value "    ///////////////////////////////////////////////////////////////////////////  ".


           05 sc-sair                  line 02  col 71 pic x(01)
           using ws-sair foreground-color 12.

           05 sc-cadastro-prova        line 05  col 10 pic x(01)
           using ws-cadastrar-prova foreground-color 15.

           05 sc-cadastro-simulado     line 06  col 10 pic x(01)
           using ws-cadastrar-simulado foreground-color 15.

      *>--------------------------------------------------------------------------------------------------------------

       01  tela-pref-usu.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "    ///////////////////////////////////////////////////////////////////////////  ".
           05 line 02 col 01 value "                                                                     [ ]Sair     ".
           05 line 03 col 01 value "                             Preferencias Usuario                                ".
           05 line 04 col 01 value "      MENU                                                                       ".
           05 line 05 col 01 value "        [ ] P - Prova                                                            ".
           05 line 06 col 01 value "        [ ] S - Simulado                                                         ".
           05 line 07 col 01 value "                                                                                 ".
           05 line 08 col 01 value "    ///////////////////////////////////////////////////////////////////////////  ".


           05 sc-sair                  line 02  col 71 pic x(01)
           using ws-sair foreground-color 12.

           05 sc-prova                 line 05  col 10 pic x(01)
           using ws-prova foreground-color 15.

           05 sc-simulado              line 06  col 10 pic x(01)
           using ws-simulado foreground-color 15.


      *>Declaração do corpo do programa
       procedure division.

       0000-controle section.

           perform 1000-inicializa
           perform 2000-processamento
           perform 3000-finaliza

           .
       0000-controle-exit.
           exit.
      *>------------------------------------------------------------------------
      *> Inicialização
      *>------------------------------------------------------------------------
       1000-inicializa section.

           open i-o arq-preferencias               *> open i-o abre o arquivo para leitura e escrita
           if     ws-fs-arq-preferencias  <> "00"
           and    ws-fs-arq-preferencias  <> "05" then
               move 1                                     to ws-msn-erro-ofsset
               move ws-fs-arq-preferencias                to ws-msn-erro-cod
               move "Erro ao abrir arq. arq-referencias"  to ws-msn-erro-text
               perform finaliza-anormal
           end-if

           .
       1000-inicializa-exit.
           exit.

      *>------------------------------------------------------------------------
      *> Processamento
      *>------------------------------------------------------------------------
       2000-processamento section.

           perform until ws-sair = "x"
                      or ws-sair = "X"

           *> inicializando variavies da tela
               move space to ws-cadastrar-prova
                             ws-cadastrar-simulado
                             ws-prova
                             ws-simulado
                             ws-sair

               display tela-pref-adm
               accept  tela-pref-usu

               if ws-cadastrar-prova = "X"
               or ws-cadastrar-prova = "x"
                   perform cadastrar-prova
               end-if

               if ws-cadastrar-simulado = "X"
               or ws-cadastrar-simulado = "x"
                   perform cadastrar-simulado
               end-if

               if ws-prova = "X"
               or ws-prova = "x"
                   perform prova
               end-if

               if ws-simulado = "X"
               or ws-simulado = "x"
                   perform simulado
               end-if

           end-perform

           .
       2000-processamento-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Cadastrar Prova
      *>------------------------------------------------------------------------
       cadastrar-prova section.

           .
       cadastrar-prova-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Cadastrar Simulado
      *>------------------------------------------------------------------------
       cadastrar-simulado section.

           .
       cadastrar-simulado-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Prova
      *>------------------------------------------------------------------------
       prova section.

           .
       prova-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Silmulado
      *>------------------------------------------------------------------------
       simulado section.

           .
       simulado-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Finalização  Anormal
      *>------------------------------------------------------------------------
       finaliza-anormal section.

           display erase
           display ws-msn-erro.
           Stop run
           .
       finaliza-anormal-exit.
           exit.


      *>------------------------------------------------------------------------
      *> Finalização Normal
      *>------------------------------------------------------------------------
       3000-finaliza section.

           close arq-preferencias
           if ws-fs-arq-preferencias  <> "00" then
               move 23                                       to ws-msn-erro-ofsset
               move ws-fs-arq-preferencias                   to ws-msn-erro-cod
               move "Erro ao fechar arq. arq-preferencias "  to ws-msn-erro-text
               perform finaliza-anormal
           end-if



           stop run
           .
       3000-finaliza-exit.
           exit.




