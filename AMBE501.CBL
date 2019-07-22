       IDENTIFICATION DIVISION.
       PROGRAM-ID.    AMBE501.
       AUTHOR.        GUALBERTO.
       DATE-WRITTEN.  21/07/2019.
       SECURITY.      *************************************************
                      *                                               *
                      *  Aborta ou Reescreve                          *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
         SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.



       WORKING-STORAGE SECTION.
       01  AREAS-DE-TRABALHO-1.
           05 wx-opcao              pic  x(001) value space.
           05 ln                    pic  9(002) value zeros.
           05 i                     pic  9(002) value zeros.
           05 NM-CODIGO             PIC  9(005) VALUE ZEROS.



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


       LINKAGE SECTION.
       01  LINKAGEM-CAMPOS.
           05  AREA-LINK                  PIC  X(100).
       PROCEDURE DIVISION USING LINKAGEM-CAMPOS.

       000-INICIO.
       000-99-FIM.
           STOP RUN.






       999-99-fim. exit.

       END PROGRAM AMBE001.