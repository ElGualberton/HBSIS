       IDENTIFICATION DIVISION.
       PROGRAM-ID.    AMBE001.
       AUTHOR.        GUALBERTO.
       DATE-WRITTEN.  21/07/2019.
       SECURITY.      *************************************************
                      *                                               *
                      *  Manutencao de clientes                       *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
         SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT txt-import ASSIGN   TO lb-txt-import
                  ORGANIZATION  IS LINE SEQUENTIAL
                  ACCESS MODE   IS SEQUENTIAL
                  FILE STATUS   IS fs-txt-import.


           select arq-cliente assign to disk wid-arq-cliente
                  organization         is indexed
                  access mode          is dynamic
                  record key           is cliente-chave
                  ALTERNATE record key is cliente-cnpj
                  ALTERNATE RECORD KEY IS cliente-razao
                                          WITH DUPLICATES
                  lock mode            is manual
                  file status          is ws-resultado-acesso.
       


           SELECT arq-cliente ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS cliente-chave
                  ALTERNATE RECORD KEY IS cliente-razao
                                          WITH DUPLICATES
                  LOCK MODE     IS AUTOMATIC
                  FILE STATUS   IS ws-resultado-acesso.

       DATA DIVISION.
       FILE SECTION.
       FD  txt-import.
       01  linha-txt-import                     pic x(1000).


      *Código Cliente    - pic  9(007);
      *CNPJ              - pic  9(014);
      *Razão Social      - pic  x(040);
      *Latitude          - pic s9(003)v9(008);
      *Longitude         - pic s9(003)v9(008);



       FD  arq-cliente
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS wid-arq-cliente.
       01  cliente-REG.
           05 cliente-chave.
              10 cliente-codigo          PIC  9(007).
           05 cliente-razao              PIC  X(040).
           05 cliente-cnpj               PIC  9(014).
           05 redefines cliente-cnpj.
              10 cliente-cnpj-empresa    PIC  9(008).
              10 cliente-cnpj-filial     PIC  9(004).
              10 cliente-cnpj-digito     PIC  9(002).
           05 cliente-latitude           PIC  s9(003)v9(008).
           05 cliente-longitude          PIC  s9(003)v9(008).


       WORKING-STORAGE SECTION.
       01  AREAS-DE-TRABALHO-1.
           05 wx-opcao              pic  x(001) value space.
           05 ln                    pic  9(002) value zeros.
           05 i                     pic  9(002) value zeros.
           05 NM-CODIGO             PIC  9(005) VALUE ZEROS.
           05 fs-txt-import         pic  x(002) value spaces.
           05 lb-txt-import         pic  x(050) value
              "\xampp\htdocs\cobolware\arq-cliente.txt-import".
           05 marcador              pic  x(002) value spaces.
           05 WS-RETORNO-TELA       PIC  X(078).
           05 MASC-VALOR            PIC  ZZZZZZZ9.99
            BLANK WHEN ZEROS.
           05 MASC-VALOR2           PIC  99999999.99
            BLANK WHEN ZEROS.
           05 REGISTRO-VALOR        PIC  X(100) VALUE SPACES.
           05 REGISTROS             PIC  9(006) VALUE 0.
           05 ws-resultado-acesso   PIC  X(002) VALUE "00".
           05 wid-arq-cliente       PIC  X(050) VALUE "arq-cliente".
       01  WS-REG.
           05 WS-CHAVE.
              10 WS-CODIGO          PIC  X(005).
           05 WS-DESCRICAO          PIC  X(030).
           05 WS-PRECO              PIC  x(011).
           05 REDEFINES WS-PRECO.
              10 WS-PRECO-CHEIO     PIC  9(008).
              10 WS-PRECO-SEPARADOR PIC  X(001).
              10 WS-PRECO-CENTAVOS  PIC  9(002).
           05 WS-TIPO               PIC  X(001).
              88 WS-PECA                 VALUE "1".
              88 WS-ACABADO              VALUE "2".
              88 WS-MATERIAL             VALUE "3".
           05 WS-OPCOES.
              10 WS-IMPORTADO       PIC  X(001).
              10 WS-GARANTIA        PIC  X(001).
              10 WS-DURAVEL         PIC  X(001).

       screen section.
       01  moldura.
         03  topo-tela.
           05  pic x(001) value     "+" line 01 col 01.
           05  pic x(078) value all "-" line 01 col 02.
           05  pic x(001) value     "+" line 01 col 80.
         03  corpo-tela.
           05  pic x(001) value     "|" line ln col 01.
           05  pic x(078) value all " " line ln col 02.
           05  pic x(001) value     "|" line ln col 80.
         03  rodape1-tela.
           05  pic x(001) value     "+" line 22 col 01.
           05  pic x(078) value all "-" line 22 col 02.
           05  pic x(001) value     "+" line 22 col 80.
         03  rodape2-tela.
           05  pic x(001) value     "+" line 24 col 01.
           05  pic x(078) value all "-" line 24 col 02.
           05  pic x(001) value     "+" line 24 col 80.
       01  constantes.
         03 pic x(020) value "CADASTRO DE CLIENTES" line 03 col 30.
         03 pic x(020) value "Codigo.......:"       line 07 col 16.
         03 pic x(020) value "Razao Social.:"       line 08 col 16.
         03 pic x(020) value "CNPJ.........:"       line 09 col 16.
         03 pic x(020) value "Latitude.....:"       line 10 col 16.
         03 pic x(020) value "Longitude....:"       line 11 col 16.
       01  variaveis.
         03 t-codigo
            line 03 col 44 
            pic Z.ZZZ.ZZ9 using cliente-codigo BLANK WHEN zeros.
         03 t-razao
            line 04 col 35
            pic x(040) using cliente-razao.
         03 t-cnpj
            line 05 col 39
            pic 9(014) using cliente-cnpj BLANK WHEN zeros.
         03 t-latitude
            line 06 col 40
            pic -ZZ9,99999999 using cliente-latitude BLANK WHEN zeros.
         03 t-longitude
            line 06 col 40
            pic -ZZ9,99999999 using cliente-longitude BLANK WHEN zeros.
       01  opcoes.
         03 pic x(058) line 21 col 12 value
         "(C)onsulta - (A)ltera - (I)nclui - (E)xclui - (V)olta Menu".
         03 pic x(070) line 23 col 03 value
         "Selecione a Opcao Desejada:  X  (Letra em parenteses das opcoe
       - "s acima)".
         03 t-opcao
            line 23 col 32
            pic x(001) using wx-opcao.
       01  pesquisa.
         03 pic x(048) line 21 col 12 value
         "Pesquisar por (C)odigo - (R)azao Social - C(N)PJ".     
       LINKAGE SECTION.
       01  LINKAGEM-CAMPOS.
           05  AREA-LINK                  PIC  X(100).
       PROCEDURE DIVISION USING LINKAGEM-CAMPOS.
       000-INICIO.
           initialize cliente-REG
                      wx-opcao.
           perform 999-monta-tela thru 999-99-fim.
           display opcoes.
           perform test after until wx-opcao not = spaces
              accept t-opcao
              move function upper-case(wx-opcao) to wx-opcao
              if wx-opcao not = "C" and "A" and "I" and "E" and "V"
                  move spaces to wx-opcao
                  display t-opcao
              end-if
           end-perform.
           OPEN I-O arq-cliente
           IF   ws-resultado-acesso > "09"
                PERFORM 900-FILE-STATUS THRU 900-99-FIM
           END-IF.

           EVALUATE wx-opcao
                when "C"
                    perform 980-pesquisa-por thru 980-99-fim
                when "A"
                    perform 980-pesquisa-por thru 980-99-fim
                    accept variaveis
                    rewrite cliente-REG
                when "E"
                    perform 980-pesquisa-por thru 980-99-fim
                    IF ws-resultado-acesso < "10"
                       DELETE arq-cliente Record
                    END-IF
                when "I"
                    initialize cliente-reg
                               NM-CODIGO
                    move 9999999 to cliente-codigo
                    start arq-cliente key is less cliente-chave
                    READ arq-cliente PREVIOUS RECORD IGNORE LOCK
                    IF ws-resultado-acesso < "10"
                       move cliente-codigo TO NM-CODIGO
                    end-if
                    ADD  1               TO NM-CODIGO
                    move NM-CODIGO       to cliente-codigo
                    accept variaveis
                    write cliente-REG
                when "V"
                    continue
           END-EVALUATE.
           perform 900-FILE-STATUS thru 900-99-FIM
           CLOSE arq-cliente.
       000-99-FIM.
           STOP RUN.

       090-INICIO-txt-import.
           open input txt-import.
           perform test after until fs-txt-import > "09"
               read txt-import
               if fs-txt-import < "10"
                   move linha-txt-import to cliente-REG
                   write cliente-REG
               end-if
           end-perform.
           close txt-import.
       090-99-FIM. EXIT.
       
       900-FILE-STATUS.
           initialize cliente-reg
           if ws-resultado-acesso = "23"
              move "Registro não encontrado." to cliente-razao
           else
              String "FileStatus "
                      ws-resultado-acesso
                      delimited by size INTO cliente-razao
           end-if.
           display t-razao
           accept t-cnpj.
           close arq-cliente.
       900-99-FIM. EXIT.

       980-pesquisa-por.
           move 23 to ln.
           display corpo-tela.
           move space to wx-opcao
           display pesquisa.
           perform test after until wx-opcao not = spaces
              accept t-opcao
              move function upper-case(wx-opcao) to wx-opcao
              if wx-opcao not = "C" and "R" and "N"
                  move spaces to wx-opcao
                  display t-opcao
              end-if
           end-perform.
           initialize cliente-REG
           evaluate wx-opcao
               when "C"
                   accept t-codigo
                   start arq-cliente key is not less cliente-chave
               when "R"
                   accept t-razao
                   start arq-cliente key is not less cliente-razao
               when "N"
                   accept t-cnpj
                   start arq-cliente key is not less cliente-cnpj
               when other
                  continue
           end-evaluate.
           read arq-cliente ignore lock.
           if ws-resultado-acesso = "00"
               display variaveis
           else
               perform 900-FILE-STATUS thru 900-99-FIM
           end-if.
       980-99-FIM. EXIT.

       999-monta-tela.
           display topo-tela.
           perform varying ln from 2 by 1 until i > 21
              display corpo-tela
           end-perform.
           display rodape1-tela.
           move 23 to ln.
           display corpo-tela.
           display rodape2-tela.
           display constantes.
           display variaveis.
       999-99-fim. exit.

       END PROGRAM AMBE001.