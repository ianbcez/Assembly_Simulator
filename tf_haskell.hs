module TF where
{- 
   Trabalho feito para a disciplina de Introdução à Ciências da Computação da turma 138 - C4606 (2016/B)
   Aunos: Bernardo Bordin and Ian Barcelos
   Pontifícia Universidade Católica do Rio Grande do Sul
-}

-- FUNÇÕES AUXILIARES--------------------------------------------------------------------------------

-- Função com o objetivo de somente auxiliar na inversão de listas para o simulador:
-- Em linguagens funcionais é necessário criar uma outra lista para armazenar a modificação
-- da resposta.
rev::[t]->[t]
rev t = reva t []   -- Uma lista auxiliar vazia para guardar a lista reserva
   where
      reva::[t]->[t]->[t]
      reva [] resp = resp  -- Se a lista for vazia, a resposta é ela mesma
      reva (h:t) resp = reva t (h:resp)  -- Tira o elemento da frente e concatena ele na resposta

-- Funções de acesso a memória----------------------------------------------------------------------
-- Pré condição: os endereços de memória existem

-- Função com o objetivo de retornar (recuperar) o conteúdo do endereço de memória da lista end:
readMem::Int->[(Int,Int)]->Int
readMem end (h:t)
      |end == (fst h) = snd h
      |end /=  (fst h) = readMem end t

 -- Função com o objetivo de armazenar (escrever) a variável val no endereço end:  
writeMem::Int -> Int -> [(Int,Int)] -> [(Int, Int)]
writeMem end val mem = aux end val mem []
   where
      aux::Int -> Int -> [(Int,Int)] -> [(Int, Int)] -> [(Int,Int)]
      aux end val [] newMem = rev newMem
      aux end val (h:t) newMem
         |(end /= (fst h)) = aux end val t (h:newMem)
         |(end == (fst h)) = aux end val t ((end,val):newMem)

-----------------------------Funções do simulador----------------------------------------------------
{--
    Busca a próxima instrução (RI)
    Retorna (codigo da instrução, parâmetro)
--}

-- Executa a operação LOAD (02)--------------------------------------------------------------------

execLOAD::Int->[(Int,Int)]->[(Int,Int)]
execLOAD end mem = writeMem 300 (readMem end mem) mem 

-- Executa a operação STORAGE(04)-------------------------------------------------------------------

execSTORAGE::Int->[(Int,Int)]->[(Int,Int)]
execSTORAGE end mem = writeMem end (readMem 300 mem) mem 

-- Executa a operação JMP(06)----------------------------------------------------------------------
--FEITA DIRETAMENTE NA EXECUÇÃO DA INSTRUÇÃO

-- Executa a operação JMZ(08)----------------------------------------------------------------------
--FEITA DIRETAMENTE NA EXECUÇÃO DA INSTRUÇÃO

-- Executa a operação CPL(10)----------------------------------------------------------------------
execCPL::Int->[(Int,Int)]->[(Int,Int)]
execCPL end mem
       |(readMem end mem <   0)   = writeMem 300 1 mem
       |(readMem end mem >= 0)   = writeMem 300 0 mem

-- Executa a operação AND(12)----------------------------------------------------------------------

execAND::Int->[(Int,Int)]->[(Int,Int)]
execAND end mem
       |(readMem end mem ==0 && readMem 300 mem ==0)  = writeMem 300 1 mem
execAND end mem = writeMem 300 0 mem

-- Executa a operação ADD(14)----------------------------------------------------------------------

execADD::Int->[(Int,Int)]->[(Int,Int)]
execADD end mem = writeMem 300 ((readMem end mem)+(readMem 300 mem)) mem 

-- Executa a operação SUB(16)------------------------------------------------------------------------

execSUB::Int->[(Int,Int)]->[(Int,Int)]
execSUB end mem = writeMem 300 ((readMem 300 mem) - (readMem end mem)) mem

-- Executa a operação NOP(18)------------------------------------------------------------------------

execNOP::Int->[(Int,Int)]->[(Int,Int)]
execNOP end mem = mem

-- Executa a operação HLT(20)------------------------------------------------------------------------
--FEITA DIRETAMENTE NA EXECUÇÃO DA INSTRUÇÃO 

--___________________________________________________________________________________________________________

-- Rotina que busca a próxima instrução (aquela indicada no Resgistrador de Memória = ri)---------------------------
-- Retorna (código da instrução, parâmetro)
fetch::Int->[(Int,Int)]->(Int,Int)
fetch ri mem = ((readMem ri mem), (readMem (ri+1) mem))

-- Decodifica a instrução agindo sobre a memória (executa a instrução)-------------------------------
-- Retorna o estado da memória depois da instrução .
-- ATENÇÃO: usa a posição 300 da memória (endereço inválido) somente para armazenar o acumulador
decoder::(Int,Int)->[(Int,Int)]->[(Int,Int)]
decoder (instr, end) mem
      |(instr == 2)  = execLOAD end mem
      |(instr == 4)  = execSTORAGE end mem
      |(instr == 10) = execCPL end mem
      |(instr == 12) = execAND end mem
      |(instr == 14) = execADD end mem
      |(instr == 16) = execSUB end mem
      |(instr == 18) = execNOP end mem

{- Executa o ciclo de execução--------------------------------------------------------------------------

Recebe a memória com programa e dados pré carregados
ATENÇÃO: usa a posição 300 da memória (endereço inválido) somente para armazenar o acumulador
Retornando o estado final da memória HLT (20).
Aqui é o bloco que tem a finalidade de execução das instruções:
executa (mem,posiçao)
-}
executa::[(Int,Int)]->[(Int,Int)]
executa mem = aux mem 0
   where
   aux::[(Int,Int)]-> Int -> [(Int,Int)]
   aux mem ri
      | fst instr == 6 = aux mem (snd instr) -- Execução do JMP(06)
      | (fst instr == 8 && readMem 300 mem == 0) = aux mem (snd instr) --Execução do JMZ(08)
      | (fst instr == 8 && readMem 300 mem /= 0) =  aux mem (ri+2) --Execução do JMZ(08)
      | fst instr /= 20 = aux (decoder instr mem) (ri+2)
      | fst instr == 20 = mem  -- Execução do Halt(20)
         where
         instr = fetch ri mem


-- Programas Básicos:
-- (A + B)-------------------------------------------------------------------------------------------

soma2Num::Int->Int->[(Int,Int)]
soma2Num a b = [(0,2),(1,240),(2,14),(3,241),(4,4),(5,251),(6,20),(240,a),(241,b),(251,0),(300,0)]

-----------------------------------------------------------------------------------------------------
-- (A - B)-------------------------------------------------------------------------------------------

sub2Num::Int->Int->[(Int,Int)]
sub2Num a b = [(0,2),(1,240),(2,16),(3,241),(4,4),(5,251),(6,20),(240,a),(241,b),(251,0),(300,0)]

-----------------------------------------------------------------------------------------------------

-- (A / B)-------------------------------------------------------------------------------------------
--LOD 241
--ADD 242
--STO 241
--LOD 251
--ADD 243
--STO 251
--CPL 241
--JMZ HLT
--JMP 0
--HLT

divNum::Int->Int->[(Int,Int)]
divNum a b = [(0,2),(1,241),(2,14),(3,242),(4,4),(5,241),(6,2),(7,251),(8,14),(9,243),(10,4),(11,251),(12,10),(13,241),(14,8),(15,18),(16,6),(17,0),(18,20),(19,18),(241,-a),(242,b),(243,1),(251,0),(300,0)]

--______________________________________________________________________________________________________________________

-- Programas Teste do PDF :    Como testar ele:    executa (programaX a b), x sendo o número do respectivo programa.

--a) A + B - 2
{--

  LOD 241
  SUB 242
  ADD 240
  STO 251
--}

programa1::Int->Int->[(Int,Int)]
programa1 a b = [(0,2),(1,241),(2,16),(3,242),(4,14),(5,240),(6,4),(7,251),(8,20),(240,a),(241,b),(242,2),(251,0),(300,0)]

-----------------------------------------------------------------------------------------------------

--b) A * B

--Programa feito excluindo a possibilidade de ser multiplicado por ZERO, ou seja, independente dos
--produtos ele sempre será feito a multiplição por UM:
{--
    LOD 241  
    ADD 242
    STO 241 
    CPL se < 0 coloca 1 , se ,maior ou igual a 0 coloca 0
    JMZ para o comando 18 se acumulador = 0
    LOD 251
    ADD 240
    STO 251
    JMP 0 , volta ao inicio
    STO 240
    HLT
--}

programa2::Int->Int->[(Int,Int)]
programa2 a b = [(0,2),(1,241),(2,14),(3,242),(4,4),(5,241),(6,10),(7,241),(8,8),(9,18),(10,2),(11,251),(12,14),(13,240),(14,4),(15,251),(16,6),(17,0),(18,4),(19,240),(20,20),(21,18),(240,a),(241,-b),(242,1),(251,a),(300,0)]

------------------------------------------------------------------------------------------------------

--c) If(A>B),Resp = A - B ; else Resp = B - A;

{--
   LOD 240
   SUB 241
   STO 251
   CPL 251
   JMZ HLT
   LOD 241
   SUB 240
   STO 251
   HLT
--}

programa3::Int->Int->[(Int,Int)]
programa3 a b = [(0,2),(1,240),(2,16),(3,241),(4,4),(5,251),(6,10),(7,251),(8,8),(9,16),(10,2),(11,241),(12,16),(13,240),(14,4),(15,251),(16,20),(17,18),(240,a),(241,b),(251,0),(300,0)]

-----------------------------------------------------------------------------------------------------


--d) A=0;Resp = 1;While(A < 10) { A = A + 1; Resp = Resp + 2}

{--
  LOD 241  
  ADD 242  
  STO 241 
  JMZ 16  
  LOD 251  
  ADD 243  
  STO 251  
  JMP 0
--}

programa4::[(Int,Int)]
programa4 = [(0,2),(1,241),(2,14),(3,242),(4,4),(5,241),(6,8),(7,16),(8,2),(9,251),(10,14),(11,243),(12,4),(13,251),(14,6),(15,0),(16,20),(17,18),(241,(-11)),(242,1),(251,1),(243,2),(300,0)]

-----------------------------------------------------------------------------------------------------




-- > 032 084 072 069 032 069 078 068 (ASCII)

-- >>> THE END