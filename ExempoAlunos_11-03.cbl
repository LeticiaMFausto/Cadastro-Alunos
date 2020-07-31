      $set sourceformat"free"

      *>Divisão de identificação do programa
       identification division.
       program-id. "ExempoAlunos_11-03".
       author. "Leticia Fausto".
       installation. "PC".
       date-written. 28/07/2020.
       date-compiled. 28/07/2020.



      *>Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>-----Declaração dos recursos externos
       input-output section.
       file-control.
                                                          *>.DAT/ geralmente o que se ultiliza é essa extensão.
           select arqCadAluno assign to "arqCadAluno.dat" *> Seleciona/cria o nome do arquivo e associa a um arquivo salvo no /C:
           organization is indexed                        *> modo em que os dados estão organizados
           access mode is dynamic                         *> modo como vou acessar eles
           lock mode is automatic                         *> Trava de segurança para poder ter mais de um usuário ao mesmo tempo sem perda de dados
           record key is fd-ind                           *> Chave de localização do ponteiro no arquivo
           file status is ws-fs-arqCadAluno.              *> File Status - Status da última operação

       i-o-control.

      *>Declaração de variáveis
       data division.

      *>----Variaveis de arquivos
       file section.
       fd arqCadAluno.  *> inicio da declração das variaveis do arquivo
       01  fd-alunos.
           05  fd-ind                              pic 9(02).
           05  fd-aluno                            pic x(25).
           05  fd-endereco                         pic x(35).
           05  fd-mae                              pic x(25).
           05  fd-pai                              pic x(25).
           05  fd-tel                              pic x(15).
           05  fd-nota.
               10  fd-nota1                        pic 9(02)v99.
               10  fd-nota2                        pic 9(02)v99.
               10  fd-nota3                        pic 9(02)v99.
               10  fd-nota4                        pic 9(02)v99.

      *>----Variaveis de trabalho
       working-storage section.

       77  ws-fs-arqCadAluno                       pic  9(02).

       01 ws-msn-erro.
          05 ws-msn-erro-ofsset                    pic 9(04).
          05 filler                                pic x(01) value "-".
          05 ws-msn-erro-cod                       pic 9(02).
          05 filler                                pic x(01) value space.
          05 ws-msn-erro-text                      pic x(42).


       01  ws-alunos.
           05  ws-ind                              pic 9(02).
           05  ws-aluno                            pic x(25).
           05  ws-endereco                         pic x(35).
           05  ws-mae                              pic x(25).
           05  ws-pai                              pic x(25).
           05  ws-tel                              pic x(15).
           05  ws-nota.
               10  ws-nota1                        pic 9(02)v99.
               10  ws-nota2                        pic 9(02)v99.
               10  ws-nota3                        pic 9(02)v99.
               10  ws-nota4                        pic 9(02)v99.


       77  ws-sair                                  pic  x(01).
           88  fechar-programa                      value "F" "f".
           88  voltar-tela                          value "V" "v".

       77  ws-menu                                  pic  x(02).





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

           open i-o arqCadAluno   *> open i-o abre o arquivo para leitura e escrita
           if ws-fs-arqCadAluno  <> 00
           and ws-fs-arqCadAluno <> 05 then
               move 1                                    to ws-msn-erro-ofsset
               move ws-fs-arqCadAluno                    to ws-msn-erro-cod
               move "Erro ao abrir arq. arqCadAluno "    to ws-msn-erro-text
               perform finaliza-anormal
           end-if





           .
       inicializa-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Processamento principal
      *>------------------------------------------------------------------------
       processamento section.

           perform until fechar-programa

               move space to ws-sair
               display "'Ca'dastrar Aluno"
               display "'Cn'Cadastrar Notas"
               display "'Cc'Consulta Cadastro"
               display "'De'letar"
               display "'Al'terar"
               accept ws-menu

               evaluate ws-menu *>direcionando a variavel menu, para suas tarefas
                   when = "Ca"
                   *>Preciso cadastrar alguma coisa antes de consultar.
                       perform cadastra-aluno

                   when = "Cn"
                       perform cadastrar-notas

                   when = "Cc"
                       perform consulta-cadastro


                   when = "De"
                       perform deletar-aluno

                    when = "Al"
                       perform alterar-aluno

                   when other
                       display "opcao invalida"
               end-evaluate

               display "'C'ontinuar"
               display "'F' para fechar programa"
               accept ws-sair
           end-perform


           .
       processamento-exit.
           exit.

       consulta-indexada section.

           display erase
           perform until voltar-tela
                      or ws-fs-arqCadAluno <> 0


      *> -------------  Ler dados do arquivo
               display "Informe o Cod. do aluno a ser consultado :"
               accept ws-ind

               move ws-ind to fd-ind
               read arqCadAluno
               if  ws-fs-arqCadAluno <> 0
              *> and ws-fs-arqCadAluno <> 10 then *>final do arquivo
                   if ws-fs-arqCadAluno = 23 then *>cod que não existe
                       display "Codigo informado invalido!"
                   else
                       move 2                                   to ws-msn-erro-ofsset
                       move ws-fs-arqCadAluno                   to ws-msn-erro-cod
                       move "Erro ao ler arq. arqCadAluno "     to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
               else

                   move  fd-alunos       to  ws-alunos

      *> -------------
                   display "Cod        : "  ws-ind
                   display "Aluno      : "  ws-aluno
                   display "Endereço   : "  ws-endereco
                   display "Pai        : "  ws-pai
                   display "Mae        : "  ws-mae
                   display "Telefone   : "  ws-tel
                   display "   "
                   display "Nota 1     : "  ws-nota1
                   display "Nota 2     : "  ws-nota2
                   display "Nota 3     : "  ws-nota3
                   display "Nota 4     : "  ws-nota4


                end-if


               display "Deseja consultar mais um aluno? 'S'im ou 'V'oltar "
               accept ws-sair

           end-perform


           .
       consulta-indexada-exit.
           exit.

       cadastrar-notas section.

           display erase
           Display "Cod. do Aluno: "
           accept ws-ind

           display "Digite a nota1 :"
           accept ws-nota1
           display "Digite a nota2 :"
           accept ws-nota2
           display "Digite a nota3 :"
           accept ws-nota3
           display "Digite a nota4 :"
           accept ws-nota4

           move ws-ind    to  fd-ind    *>Direcionando o ponteiro para saber em que ind eu vou gravar.
           read arqCadAluno
           if ws-fs-arqCadAluno <> 0 then   *>Aqui fazendo teste de erros para saber que deu algo errado na leitura
               if ws-fs-arqCadAluno = 23 then
                   display "Cod. de Aluno não existe"
               else
                   move 7                                  to ws-msn-erro-ofsset
                   move ws-fs-arqCadAluno                  to ws-msn-erro-cod
                   move "Erro ao ler arq. arqCadAluno "    to ws-msn-erro-text
                   perform finaliza-anormal
               end-if
           else
               move ws-nota   to fd-nota
               rewrite fd-alunos            *> reescreevendo no fd-alunos sem sobreescrever as informações já contidas
               if ws-fs-arqCadAluno <> 0 then
                   move 7                                   to ws-msn-erro-ofsset
                   move ws-fs-arqCadAluno                   to ws-msn-erro-cod
                   move "Erro ao gravar arq. arqCadAluno "  to ws-msn-erro-text
                   perform finaliza-anormal
               end-if

           end-if


           .
       cadastrar-notas-exit.
           exit.




      *>------------------------------------------------------------------------
      *>  Rotina de cadastro de alunos  - escreve no arquivo
      *>------------------------------------------------------------------------
       cadastra-aluno section.

           display erase
           perform until voltar-tela
                      or (ws-fs-arqCadAluno <> 0
                      and ws-fs-arqCadAluno <> 5)


               perform busca-proximo-cod

               display "Cod        : " ws-ind
               display "Aluno      : "
               accept  ws-aluno
               display "Endereço   : "
               accept  ws-endereco
               display "Nome do Pai: "
               accept  ws-pai
               display "Nome da Mãe: "
               accept  ws-mae
               display "Telefone   : "
               accept  ws-tel


      *> -------------  Salvar dados no arquivo
               move  ws-alunos       to  fd-alunos

               write fd-alunos *> grava os dados no arquivo
               if ws-fs-arqCadAluno <> 0 then   *>SEMPRE TESTAR O FILE STATUS
                   display "File Status ao gravar arquivo: " ws-fs-arqCadAluno
               end-if
      *> -------------


               display "Deseja cadastrar mais um aluno? 'S' ou 'V'oltar"
               accept ws-sair



           end-perform

           .
       cadastra-aluno-exit.
           exit.


       busca-proximo-cod section.


           move 1 to fd-ind
           start arqCadAluno *>posicionando o ponteiro e evitando o file status 46
           if ws-fs-arqCadALuno = 0 then
               perform until ws-fs-arqCadAluno = 10
                   read arqCadAluno next  *> Lê os dados do arquivo
                   if ws-fs-arqCadALuno <> 0
                   and  ws-fs-arqCadAluno <> 10 then *>10 é fim do arquivo
                       move 7                                   to ws-msn-erro-ofsset
                       move ws-fs-arqCadAluno                   to ws-msn-erro-cod
                       move "Erro ao ler arq. arqCadAluno "     to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
               end-perform
               move  fd-ind       to  ws-ind
               add 1              to ws-ind
           else
               if ws-fs-arqCadAluno = 23 then  *> file status 23 é cog inexistente, isso quando é a primeira vez que eu entro.(tratamento)
                   move 1 to fd-ind
               else
                   move 7                                   to ws-msn-erro-ofsset
                   move ws-fs-arqCadAluno                   to ws-msn-erro-cod
                   move "Erro ao  arq. arqCadAluno "     to ws-msn-erro-text
                   perform finaliza-anormal
                end-if
           end-if




           .
       busca-proximo-cod-exit.
           exit.



       consulta-cadastro section.


           display "Informe 'I' para Consulta Indexada  : "
           display "Informe 'S' para Consulta Sequencial: "
           accept ws-menu

           evaluate ws-menu

               when = "I"
                       perform consulta-indexada

               when = "S"
                       perform consultar-sequencial-next

               when other
                   display "Opcao Invalida"
           end-evaluate


           .
       consulta-cadastro-exit.
           exit.

       consultar-sequencial-next section.

           move 1 to ws-ind
           perform until voltar-tela


               read arqCadAluno next
               if  ws-fs-arqCadAluno <> 0  then
                  if ws-fs-arqCadAluno = 10 then
                      perform consultar-sequencial-prev
                  else
                      move 3                                   to ws-msn-erro-ofsset
                      move ws-fs-arqCadAluno                   to ws-msn-erro-cod
                      move "Erro ao ler arq. arqCadAluno "     to ws-msn-erro-text
                      perform finaliza-anormal
                  end-if


               move  fd-alunos       to  ws-alunos

      *> -------------
                   display "Cod        : "  ws-ind
                   display "Aluno      : "  ws-aluno
                   display "Endereço   : "  ws-endereco
                   display "Pai        : "  ws-pai
                   display "Mae        : "  ws-mae
                   display "Telefone   : "  ws-tel
                   display "   "
                   display "Nota 1     : "  ws-nota1
                   display "Nota 2     : "  ws-nota2
                   display "Nota 3     : "  ws-nota3
                   display "Nota 4     : "  ws-nota4
               end-if


               display "Deseja consultar mais um Cadastro? 'S' ou 'V'oltar"
               accept ws-sair



           end-perform


           .
       consultar-sequencial-next-exit.
           exit.


       consultar-sequencial-prev section.

           perform until voltar-tela


               read arqCadAluno previous
               if  ws-fs-arqCadAluno <> 0  then
                  if ws-fs-arqCadAluno = 10 then
                      perform consultar-sequencial-next
                  else
                      move 3                                   to ws-msn-erro-ofsset
                      move ws-fs-arqCadAluno                   to ws-msn-erro-cod
                      move "Erro ao ler arq. arqCadAluno "     to ws-msn-erro-text
                      perform finaliza-anormal
                  end-if


               move  fd-alunos       to  ws-alunos

      *> -------------
                   display "Cod        : "  ws-ind
                   display "Aluno      : "  ws-aluno
                   display "Endereço   : "  ws-endereco
                   display "Pai        : "  ws-pai
                   display "Mae        : "  ws-mae
                   display "Telefone   : "  ws-tel
                   display "   "
                   display "Nota 1     : "  ws-nota1
                   display "Nota 2     : "  ws-nota2
                   display "Nota 3     : "  ws-nota3
                   display "Nota 4     : "  ws-nota4
               end-if


               display "Deseja consultar mais um Cadastro? 'S' ou 'V'oltar"
               accept ws-sair



           end-perform


           .
       consultar-sequencial-prev-exit.
           exit.


       deletar-aluno section.


      *> -------------  Apagar dados do registro do arquivo
               display "Informe o Cod. do Aluno a ser excluido:"
               accept ws-ind

               move ws-ind to fd-ind
               delete arqCadAluno
               if  ws-fs-arqCadAluno = 0 then
                   display "Aluno " ws-ind " apagado com sucesso!"
               else
                   if ws-fs-arqCadAluno = 23 then
                       display "Cod. informado invalido!"
                   else
                       if ws-fs-arqCadAluno <> 0 then
                           move 5                                   to ws-msn-erro-ofsset
                           move ws-fs-arqCadAluno                   to ws-msn-erro-cod
                           move "Erro ao apagar arq. arqCadAluno "  to ws-msn-erro-text
                           perform finaliza-anormal
                       end-if
                   end-if
               end-if
           .
       deletar-aluno-exit.
           exit.


       alterar-aluno section.

           display "Informe o Cod do Aluno"
           accept ws-ind

           move ws-ind   to fd-ind
           read arqCadAluno
           if  ws-fs-arqCadAluno <> 0
               if ws-fs-arqCadAluno = 23 then *>cod que não existe
                       display "Codigo informado invalido!"
                   else
                       move 2                                   to ws-msn-erro-ofsset
                       move ws-fs-arqCadAluno                   to ws-msn-erro-cod
                       move "Erro ao ler arq. arqCadAluno "     to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
               else

                   move fd-alunos  to ws-alunos

                   display " '1' Para Aluno"
                   display " '2' Para Endereco "
                   display " '3' Para Nome Mae "
                   display " '4' Para Nome Pai"
                   display " '5' Para Telefone "
                   display " '6' Para Notas"
                   accept ws-menu



                   evaluate ws-menu

                       when = "1"
                           display "Nome do Aluno: "
                           accept ws-aluno
                       when = "2"
                           display "Endereco: "
                           accept  ws-endereco
                       when = "3"
                           display "Nome do Pai: "
                           accept  ws-pai

                       when = "4"
                           display "Nome da Mãe: "
                           accept  ws-mae

                       when = "5"
                           display "Novo Telefone: "
                           accept  ws-tel

                       when = "6"
                           display " '1' Para Aluno"
                           display " '2' Para Endereco "
                           display " '3' Para Nome Mae "
                           display " '4' Para Nome Pai"
                           accept ws-menu

                           evaluate ws-menu

                               when = "1"
                                   display " Nota 1: "
                                   accept ws-nota1

                               when = "2"
                                   display " Nota 2: "
                                   accept ws-nota2

                               when = "3"
                                   display " Nota 3: "
                                   accept ws-nota3

                               when = "4"
                                   display " Nota 4: "
                                   accept ws-nota4

                           end-evaluate


                       when other
                       display "Opcao Invalida"

                   end-evaluate

               move ws-alunos         to fd-alunos

               rewrite fd-alunos
               if  ws-fs-arqCadAluno = 0 then
                   display "Cadastro do Aluno " ws-ind " alterado com sucesso!"
               else
                   if ws-fs-arqCadAluno <> 0 then
                       move 6                                   to ws-msn-erro-ofsset
                       move ws-fs-arqCadAluno                   to ws-msn-erro-cod
                       move "Erro ao alterar arq. arqTemp "     to ws-msn-erro-text
                       perform finaliza-anormal
                   end-if
               end-if
           .
       alterar-aluno-exit.
           exit.



       finaliza-anormal section.
           display erase
           display ws-msn-erro.
           Stop run
           .
       finaliza-anormal-exit.
           exit.

      *>------------------------------------------------------------------------
      *>  Finalização
      *>------------------------------------------------------------------------
       finaliza section.

           close arqCadAluno
           if ws-fs-arqCadAluno <> 0 then
               move 8                                  to ws-msn-erro-ofsset
               move ws-fs-arqCadAluno                  to ws-msn-erro-cod
               move "Erro ao fechar arq. arqCadAluno " to ws-msn-erro-text
               perform finaliza-anormal
           end-if


           Stop run
           .
       finaliza-exit.
           exit.













