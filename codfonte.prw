#INCLUDE "FINA050.CH"
#INCLUDE "PROTHEUS.CH"
#INCLUDE "DBINFO.CH"
#INCLUDE "MSMGADD.CH"
#INCLUDE "FWMVCDEF.CH"
#INCLUDE "FWADAPTEREAI.CH"
#INCLUDE "XMLXFUN.CH"
#include "FILEIO.CH"
#INCLUDE "FWLIBVERSION.CH"

Static __cLocTit As Character

Static __lCodFil  := .T.
Static __lPmsInt  := NIL
Static __lIsIssBx := Nil
Static __dLastPCC := CTOD("22/06/2015")
Static __lRatDes  := .F.
Static __oFIN0501 := NIL
Static __cFIN1Name:= ""
Static __cChTitDs := ""
Static __lFA50UPD := Nil
Static __lF50PROV := NIL
Static __lFNCDRET := NIL
Static __lF50TMP1 := Nil
Static __lF50HEAD := NIL
Static __nTamFor  := NIL
Static __cForPar  := NIL
Static __nTaEHNum := NIL
Static __lRatMNat := .F.
Static __lF50CMNT := Nil
Static __lPLSFN50 := NIL
Static __lIntPFS  := Nil
Static __lBtrISS  := Nil
Static __lFIN50VA := NIL
Static __lHasEAI  := NIL
Static __aVAAuto  := NIL
Static __lTemMR   := Nil
Static __aVetImp  := {}
Static __lPccMR   := .F.
Static __lIrfMR   := .F.
Static __lInsMR   := .F.
Static __lIssMR   := .F.
Static __lCidMR   := .F.
Static __lSestMR  := .F.
Static __lOtImpMR := .F.
Static __nVlrMR   := 0
Static __lPccBxMR := .F.
Static __lIrfBxMR := .F.
Static __lGrvMR   := .F.
Static __nImpMR   := 0
Static __lRatDsd  := NIL
Static __lInsPrev := NIL
Static __lPlOpeLt := NIL
Static __lLocBRA  := cPaisLoc == "BRA"
Static __oFIN0502 := NIL
Static __cFIN2Name:= NIL
Static __aPesqui  := {}
Static __aCbx	  := {}
Static __aIndices := {}
Static __aIniCpos := {}
Static __lFa050S  := NIL
Static __lF050SUB := NIL
Static __SelPai   := .F.
Static __oAbtQry  := NIL
Static __oRatIRF  := NIL
Static __lRateioIR := .F.
Static __oRatQry  := NIL
Static __lNRasDSD := Nil
Static __nOrdOk   := 0
Static __lMotBx   := .T.
Static __lLimMot  := Nil
Static __aTitCalc := {}
Static __lFinLDCB := NIL
Static __lMetric  := NIL
Static __cFunBkp  := ""
Static __cFunMet  := ""
Static __lFnBtr   := Nil
Static __aRestric := { "E2_PREFIXO","E2_NUM","E2_PARCELA","E2_TIPO","E2_NATUREZ","E2_FORNECE","E2_LOJA","E2_NOMFOR","E2_EMISSAO",;
                    "E2_VENCTO","E2_VENCREA","E2_SALDO","E2_ACRESC","E2_DECRESC","E2_MOEDA","E2_HIST","E2_FILORIG" }

Static __oHsREINF := Nil // Lista de informações do Título
Static __lFlagFKF := .F.
Static __lGesplan 	:= Nil
Static __lFa986Vld	As Logical
Static __lDedSimpl  As Logical 
Static __lMotInDb As Logical

//-------------------------------------------------------------------
/*/{Protheus.doc}FINA050
Programa p/ manutenção Contas a Pagar

@author Wagner Xavier
@since  27/04/92
/*/
//-------------------------------------------------------------------
Function FINA050(aRotAuto As Array, nOpcion As Numeric, nOpcAuto As Numeric, bExecuta As Block, aDadosBco As Array, lExibeLanc As Logical,;
			     lOnline As Logical, aDadosCTB As Array, aTitPrv As Array, lMsBlQl As Logical, lPaMovBco As Logical, aVAAutP As Array, cUUIDBS As Character) 

	Local aAreaAVG    as Array
    Local aCampoAUT   as Array
    Local aCmpSE2     as Array
	Local aFKFLoc     as Array
	Local aFKGLoc     as Array
	Local aTamSX3     as Array
	Local bBlock      as Block
    Local cBcoVld     as Character
    Local cCmpAUT     as Character
    Local cCondicao1  as Character
	Local cKey1       as Character
	Local cParcela    as Character 
    Local lExistX3    as Logical
	Local lF050BROW   as Logical
	Local lF050FILB   as Logical
	Local lF50PERGUNT as Logical
	Local lFKF        as Logical
	Local lFKG        as Logical
    Local lPanelFin   as Logical
	Local lRAGPE      as Logical
	Local lRet        as Logical
    Local lTms        as Logical
	Local nIndex1     as Numeric
	Local nPos        as Numeric
	Local nPosEv      as Numeric
	Local nPosImp     as Numeric
	Local nPosMoed    as Numeric
	Local nPosTit     as Numeric
	Local nTAAge      as Numeric
	Local nTABco      as Numeric
	Local nTAChq      as Numeric 
	Local nTACnt      as Numeric
	Local nTAMoe      as Numeric 
	Local nTamParc    as Numeric
	Local nX          as Numeric
    Local lGet105MVC  as Logical

	PRIVATE aDadosImp  as Array
	PRIVATE aDadosRef  as Array
	PRIVATE aDadosRet  as Array
	PRIVATE cModRetPIS as Character
	Private Valor5     as Numeric
	Private Valor6     as Numeric
	Private Valor7     as Numeric
    Private cOldNaturez     as Character
	PRIVATE aAutoCab        as Array
	Private aRatEvEz        as Array
	PRIVATE aTrocaF3        as Array
	PRIVATE cHistDsd        as Character
	Private cSE2TpDsd       as Character
	Private cTipoParaAbater as Character
	PRIVATE lAlterNat       as Logical
	PRIVATE lAltTxMoeda     as Logical
	PRIVATE lAltValor       as Logical
	Private nDifPcc         as Numeric
	PRIVATE nIrrfAnt        as Numeric
    Private nOldTxMoeda     as Numeric
	Private nOldValorPg     as Numeric
	Private nRecnoNdf       as Numeric
	Private nValDig         as Numeric
    
    Default __lLimMot  := ExistFunc("LimMotRead")    
    Default __lFnBtr   := FindFunction("ISSCPOM") .And. FindFunction("BtrISSMun")
    Default __lBtrISS  := SE2->(ColumnPos("E2_BTRISS")) > 0 .And. SE2->(ColumnPos("E2_VRETBIS")) > 0 .And. SE2->(ColumnPos("E2_CODSERV")) > 0 .And. __lFnBtr

	lPanelFin := IsPanelFin()
	cKey1     := ""
	cCondicao1:= ""
	nIndex1   := 0
	nPos      := 0
	bBlock    := {||}
	nX        := 0
	cParcela  := Chr(Asc(GetMV("MV_1DUP"))-1)
	nTamParc  := TAMSX3("E2_PARCELA")[1]
	aAreaAVG  := {}
	lRAGPE	  := .F.
	nPosEv	  := 0 //posição do array do rateio multinaturezas
	nPosTit	  := 0
	nPosImp	  := 0
	nTABco    := 0
	nTAAge    := 0
	nTACnt    := 0
	nTAMoe    := 0
	nTAChq    := 0
	lFKG      := .F.
	lFKF 	  := .F.
	aFKFLoc	  := {}
	aFKGLoc	  := {}
	lRet	  := .T.
	aTamSX3	  := {}
	lF050BROW := ExistBlock("F050BROW")
	lF050FILB := ExistBlock("F050FILB")
	nPosMoed  := 0
	lF50PERGUNT := ExistBlock("F50PERGUNT")
    aCampoAUT := {}
    aCmpSE2   := {}
    cCmpAUT   := "AUTRATEEV|AUTCMTIT|AUTCMIMP|AUTBANCO|AUTAGENCIA|AUTCONTA|AUTMOED|AUTCHEQUE|CCHEQUEADT|AUTCDPGDSD|AUTPERIDSD|AUTTOPADSD|AUTNPARDSD|AUTHISTDSD|AUTRATAFR"
    lExistX3  := .T.
    cBcoVld   := ""
    lTms      := nModulo == 43

	SaveInter() // Salva variaveis publicas

	Valor5      := 0
	Valor6      := 0
	Valor7      := 0
	cModRetPIS  := GetNewPar( "MV_RT10925", "1" )
	aDadosRef   := Array(7)
	aDadosRet   := Array(7)
	aDadosImp   := Array(3)
	cOldNaturez := ""
	lAlterNat   := .F.
	nRecnoNdf   := 0
	nDifPcc     := 0
	nOldValorPg := 0
    nOldTxMoeda	:= 0
	lAltValor   := .F.
	aAutoCab    := AClone(aRotAuto)
	aTrocaF3    := {}
	aRatEvEz    := nil
	cSE2TpDsd   := ""  // variável utilizada pelo PMS
	cTipoParaAbater := ""
	cHistDsd	:= CRIAVAR("E2_HIST",.F.)  // Historico p/ Desdobramento
	nValDig     := 0 //armazena valor digitado na inclusão, para restaurar o E2_VALOR, caso ocorra mudança de moeda
	lAltTxMoeda := .F.
	nIrrfAnt	:= 0

	If cPaisLoc $ "ARG|POR|EUA"
		Private cIndice
		Private cIndexArg
	Endif

	PRIVATE lIntegracao := IF(GetMV("MV_EASYFIN")=="S",.T.,.F.)
	//Campo para alimentar o campo E2_EMIS1
	PRIVATE dDataEmis1	:= dDataBase

	// Restringe o uso do programa ao Financeiro,Sigaloja e Photo
	If !(AmIIn(5,6,7,11,12,14,41,97,17,44,69,72))           // S¢ Fin,GPE, Vei, Loja , Ofi, Pecas e Esp, EIC, GCT
		Return
	Endif

    lGet105MVC  := CTB105MVC() // Guarda o Valor da Static CTBA105:CTB105MVC():lRotMVC
    CTB105MVC(.T.)  // Atribui Valor a Static CTBA105:CTB105MVC():lRotMVC ==> Passar pela validação antiga 

	// Campos especificos e documentados para uso na MSMM disponivel no Quark e utilizados em clientes
	// Nao retirar o FieldPos
	If SE2->(FieldPos("E2_CODOBS")) > 0
		Private aMemos := { { "E2_CODOBS" , "E2_OBS" } }
	Endif

	// Define Variaveis
	PRIVATE nOldValor := 0
	PRIVATE nOldSaldo := 0
	PRIVATE nOldISS	:= 0
	PRIVATE nOldBtrISS := 0
	PRIVATE nOldIRR	:= 0
	PRIVATE nOldInss  := 0
	PRIVATE nOldSEST  := 0
	PRIVATE nValorAnt := 0
	PRIVATE nMaxParc  := 0
	PRIVATE nOldPis	:= 0
	PRIVATE nOldCofins:= 0
	PRIVATE nOldCsll	:= 0
	PRIVATE nOldCID   := 0
	PRIVATE nVlRetPis	:= 0
	PRIVATE nVlRetCof := 0
	PRIVATE nVlRetCsl	:= 0

	If Type("aColsSev") != "A" .Or. Type("aHeaderSev") != "A"
		PRIVATE aColsSev	:= {} // Utilizada em MultNat2 e GravaSev
		PRIVATE aHeaderSev	:= {} // Utilizada em MultNat2 e GravaSev
	Endif

	//Variavel para indicar se o fornecedor pessoa juridica deve utilizar a tabela progressiva de IRRF
	PRIVATE lIRProg	:= "2"

	PUBLIC N // para o mvc

    // usado para controlar uma única execução por pilha de chamadas.
    // pois pode ser chamado várias vezes pelo FINA750 ou entrando e saindo da FINA050.
    // A rotina ReadMotBx armazena arquivo em variavel Statica e não lê novamente  
    // depois que foi incluído, até Variaveis Estaticas serem reiniciadas.
    if __lMotBx
        FSubsMotBx("STP","SUBSTPR   ","PNNN")
        if __lLimMot
        	//Limpa variável static __aMotRead do fonte matxfunb.prx para carregamento do array novamente
	        LimMotRead()
        endif
        __lMotBx := .F.
    endif

	// Trata o tamanho dos campos passados.
	If ValType(aAutoCab) = "A"
		For nX := 1 to len(aAutoCab)
			If ValType(aAutoCab[nX]) = "A" .and. len(aAutoCab[nX]) > 1 .and. ValType(aAutoCab[nX, 1]) = "C"
				aTamSX3 := TamSX3(aAutoCab[nX, 1])
				If len(aTamSX3) > 2 .and. aTamSX3[3] = "C"
					aAutoCab[nX, 2] := PadR(aAutoCab[nX, 2], aTamSX3[1])
				Endif
			Endif
		Next nX
	Endif

	If FwIsInCallStack("FWMILEProA")
		//Verifica se veio como array os parametros vindos do mile, pois o mesmo envia somente array
		If ValType(nOpcion) == "A"
			If Len(nOpcion) > 0
				nOpcion := nOpcion[1]
			Else
				nOpcion := nil
			EndIf
		EndIf

		If ValType(nOpcAuto) == "A"
			If Len(nOpcAuto) > 0
				nOpcAuto := nOpcAuto[1]
			Else
				nOpcAuto := 3
			EndIf
		EndIf

		If ValType(bExecuta) == "A"
			If Len(bExecuta) > 0
				bExecuta := bExecuta[1]
			Else
				bExecuta := nil
			EndIf
		EndIf

		If ValType(lExibeLanc) == "A"
			If Len(lExibeLanc) > 0
				lExibeLanc := lExibeLanc[1]
			Else
				lExibeLanc := nil
			EndIf
		EndIf

		If ValType(lOnline) == "A"
			If Len(lOnline) > 0
				lOnline := lOnline[1]
			Else
				lOnline := nil
			EndIf
		EndIf

		If ValType(lMsBlQl) == "A"
			If Len(lMsBlQl) > 0
				lMsBlQl := lMsBlQl[1]
			Else
				lMsBlQl := nil
			EndIf
		EndIf

		If ValType(lPaMovBco) == "A"
			If Len(lPaMovBco) > 0
				lPaMovBco := lPaMovBco[1]
			Else
				lPaMovBco := nil
			EndIf
		EndIf
	EndIf

	aFill(aDadosRef,0)
	aFill(aDadosRet,0)
	aFill(aDadosImp,0)

	If cPaisLoc == 'ARG'
		nMaxParc := SuperGetMV("MV_LIMCUOT",.T.,0)
	Else
		If nTamParc == 1  // TAMANHO DA PARCELA
			For nX := 1 To 63
				cParcela:=Soma1( cParcela,, .F.,.T. )
				If cParcela == "000000" .or. cParcela == "*"
					Exit
				Endif
				nMaxParc++
			Next
		Else
			Do Case
				Case nTamParc == 2
					nMaxParc := 926
				Case nTamParc > 2
					nMaxParc := 35658
			EndCase
		Endif
	EndIf

	// Cria indice condicional para a Localizacao Argentina.
	If cPaisLoc $ "ARG|POR|EUA" .And. nOpcion # Nil
		if nOpcion==1  
			cCondicao1	:=	"Alltrim(E2_TIPO)=='CH' .OR. Alltrim(E2_TIPO)=='TF'"
		Else
			cCondicao1	:=	"!(Alltrim(E2_TIPO)=='CH' .OR. Alltrim(E2_TIPO)=='TF')"
		Endif

		cIndexArg 	:= CriaTrab(Nil,.F.)
		cKey1		:=	"E2_FILIAL+E2_FORNECE+E2_LOJA"
		IndRegua("SE2",cIndexArg,cKey1,,cCondicao1,STR0079)  //"Un Momento por favor..."
		cIndice:='Proveedor+Sucursal'
		nIndex1 	:= RetIndex("SE2")
		dbSelectArea("SE2")

		dbSetOrder(nIndex1+1)
		dbGoTop()
	Endif

	Private aRotina := MenuDef(nOpcion)
	Private lF050Auto := ( aRotAuto <> NIL )

	If lF050Auto
		nPosEv := AScan(aAutocab,{|x|AllTrim(x[1])=="AUTRATEEV"})
		If nPosEv > 0
			aRatEvEz := aClone(aAutoCab[nPosEv][2])
		Endif

        nPosTit := AScan(aAutocab,{|x|AllTrim(x[1])=="AUTCMTIT"})
		If nPosTit > 0
			aFKFLoc := aClone(aAutoCab[nPosTit][2])
			lFKF := .T.
		Endif

		nPosImp := AScan(aAutocab,{|x|AllTrim(x[1])=="AUTCMIMP"})
		If nPosImp > 0
			aFKGLoc := aClone(aAutoCab[nPosImp][2])
			lFKG := .T.
		Endif

		// Valores acessórios - rotina automatica CP
		If (aVAAutP <> Nil )
			__aVAAuto := aClone(aVAAutP)
		Endif
	Endif

	// Carrega funcao Pergunte
	If !lF050Auto
		SetKey (VK_F12,{|a,b| AcessaPerg("FIN050",.T.)})
	Endif

	pergunte("FIN050",.F.)

	IF lF050Auto .And. lF50PERGUNT
		ExecBlock("F50PERGUNT", .F., .F.)
	EndIf

	//Tratamento para evitar conflito do pergunte FIN050
	//com o TMA250 ao gerar contrato de carreteiro - TMS
	If lExibeLanc <> NIL .And. ValType(lExibeLanc) == "L"
		mv_par01 := Iif(lExibeLanc,1,2) //Exibe Lancamentos Contabeis
	EndIf

	//Tratamento para evitar conflito do pergunte FIN050
	//com o TMA250 ao gerar contrato de carreteiro - TMS
	If lOnline <> NIL .And. ValType(lOnline) == "L"
		mv_par04 := Iif(lOnline,1,2) //Contabiliza On-Line
	EndIf

	// Define o cabecalho da tela de atualizacoes
	PRIVATE cCadastro 	:= STR0007 // "Contas a Pagar"
	PRIVATE cBancoAdt	:= CriaVar("A6_COD")
	PRIVATE cAgenciaAdt	:= CriaVar("A6_AGENCIA")
	PRIVATE cNumCon	 	:= CriaVar("A6_NUMCON")
	PRIVATE nMoedAdt	:= CriaVar("A6_MOEDA")
	PRIVATE cChequeAdt	:= CriaVar("EF_NUM")
	PRIVATE cHistor		:= CriaVar("EF_HIST")
	PRIVATE cBenef		:= CriaVar("EF_BENEF")
	PRIVATE lAltera		:= .F.
	PRIVATE nMoeda 		:= Int(Val(GetMv("MV_MCUSTO")))
	If Type("lWserver") == "U"
		PRIVATE cMarca 		:= GetMark( )
	EndIf
	PRIVATE aTELA[0][0]
	PRIVATE aGETS[0]
    PRIVATE cPictHist   := ""
	PRIVATE lVerifyBlq  := .F.
	PRIVATE cLote       := ""
	PRIVATE nQtdTot     := 0		//Utilizado no Rateio Externo do SIGACTB.
	PRIVATE aItensCTB   := Iif(aDadosCTB <> Nil, aDadosCTB, {})
	PRIVATE aItnTitPrv  := Iif(aTitPrv   <> Nil, aTitPrv  , {})
	PRIVATE cItnUuidBs  := Iif(cPaisLoc = "RUS" .AND. CUUIDBS <> Nil, cUUIDBS, "") //cItnUuidBs - UUID Bank statment insinde FINA050
	DEFAULT aDadosBco   := {}
	DEFAULT lMsBlQl     := .T.
	DEFAULT lPaMovBco	:= .T.
	DEFAULT cUUIDBS	    := ""
    
	lVerifyBlq := lMsBlQl

	If !lPaMovBco
		mv_par05 := 2 //-- Gera Chq. para Adiantamento == Nao
		mv_par09 := 2  //-- Somente gera movimento apos geracao do cheque

	ElseIf lTms .And. Len(aDadosBco) > 0

		cBancoAdt	:= aDadosBco[1]
		cAgenciaAdt	:= aDadosBco[2]
		cNumCon	 	:= aDadosBco[3]
		cChequeAdt	:= aDadosBco[4]

		mv_par05    := 1 //-- Gera Chq. para Adiantamento == Sim

		If Len(aDadosBco) > 6
			If aDadosBco[7]   //-- Mov. Bancario sem Cheque
				mv_par09 := 1  //-- Gera movimento sem cheque
				mv_par05 := 2 //-- Gera Chq. para Adiantamento == Nao
			Else
				mv_par05 := 2 //-- Gera Chq. para Adiantamento == Nao
				mv_par09 := 2  //-- Somente gera movimento apos geracao do cheque
			EndIf
		Else
			mv_par09 := 1
		EndIf

	EndIf

	If lF050Auto
		If (nPosMoed := AScan(aAutocab, {|x|AllTrim(x[1]) == "E2_MOEDA"})) > 0 .And. !Empty(aAutocab[nPosMoed, 2])
			nMoeda := If(ValType(aAutocab[nPosMoed, 2]) == "N", aAutocab[nPosMoed, 2], Val(aAutocab[nPosMoed, 2]))
		EndIf

		aValidGet := {}
		IF (nT := ascan(aRotAuto,{|x| x[1]='E2_TIPO'}) ) > 0
			IF aRotAuto[nT,2] $ MVPAGANT // Se for PA 
				IF (nTABco := ascan(aRotAuto,{|x| x[1]='AUTBANCO'})) > 0
					Aadd(aValidGet,{'cBancoAdt' ,PAD(aRotAuto[nTABco,2],TamSx3("E5_BANCO")[1]),"CarregaSa6(@cBancoAdt,,,.T.)",.t.})
                elseif (nTABco := ascan(aRotAuto,{|x| x[1]='AUTBANCO'})) = 0 .and. nOpcauto <> 5 .AND. !lTms
                    cBcoVld := ".F."
                    cBancoAdt := ""
                    Aadd(aValidGet,{'cBancoAdt' ,cBancoAdt,cBcoVld,.t.})
				Endif

				IF (nTAAge := ascan(aRotAuto,{|x| x[1]='AUTAGENCIA'}) ) > 0
					Aadd(aValidGet,{'cAgenciaAdt' ,PAD(aRotAuto[nTAAge,2],TamSx3("E5_AGENCIA")[1]),"CarregaSa6(@cBancoAdt,@cAgenciaAdt,,.T.)",.t.})
                elseif (nTAAge := ascan(aRotAuto,{|x| x[1]='AUTAGENCIA'}) ) = 0 .and. nOpcauto <> 5 .AND. !lTms
                    cBcoVld := ".F."
                    cAgenciaAdt := ""
                    Aadd(aValidGet,{'cAgenciaAdt',cAgenciaAdt,cBcoVld,.t.})
				EndIf

				IF (nTACnt := ascan(aRotAuto,{|x| x[1]='AUTCONTA'}) ) > 0
					Aadd(aValidGet,{'cNumCon' ,PAD(aRotAuto[nTACnt,2],TamSx3("E5_CONTA")[1]),"CarregaSa6(@cBancoAdt,@cAgenciaAdt,@cNumCon,.F.,,.T.)",.t.})
                elseif (nTACnt := ascan(aRotAuto,{|x| x[1]='AUTCONTA'}) ) = 0 .and. nOpcauto <> 5 .AND. !lTms
                    cBcoVld := ".F."
                    cNumCon := ""
                   Aadd(aValidGet,{'cNumCon',cNumCon,cBcoVld,.t.})
				EndIf

				If FXMultSld()
					If ( nTAMoe := aScan( aRotAuto, { |x| x[1] = 'AUTMOED' } ) ) > 0
						aAdd( aValidGet, { 'nMoedAdt', Pad( aRotAuto[nTAMoe,2], TamSx3("A6_MOEDA")[1]),"CarregaSa6(@cBancoAdt,@cAgenciaAdt,@cNumCon,.F.,,.T.,, @nMoedAdt )",.t.})
					EndIf
				EndIf

				IF (nTAChq := ascan(aRotAuto,{|x| x[1]='AUTCHEQUE'}) ) > 0
					If mv_par05 == 1 .And. substr(cBancoAdt,1,2)!="CX" .And. !(cBancoAdt $ GEtMV("MV_CARTEIR"))
						Aadd(aValidGet,{'cChequeAdt' ,aRotAuto[nTAChq,2],"fa050Cheque(cBancoAdt,cAgenciaAdt,cNumCon,cChequeAdt,Iif(cPaisLoc $ 'ARG|MEX',.F.,.T.))",.t.})
					Endif
				EndIf
				If ! SE2->(MsVldGAuto(aValidGet)) // consiste os gets
					lRet:= .F.
				EndIf
			Endif
		Endif

        For nX := 1 To Len(aAutocab)
            If aAutocab[nX,1] $ cCmpAUT
                AAdd( aCampoAUT, aAutocab[nX] )
            Else
                //Retorna o tipo do campo no SX3 ou vazio quando não encontra o campo
                lExistX3 := !Empty(FWSX3Util():GetFieldType( aAutocab[nX,1] ) )

                If lExistX3
                    AAdd( aCmpSE2, aAutocab[nX] )
                Endif
            EndIf
        Next nX

        // Ordeno o array da execauto para efetuar os calculos de impostos corretamente.
        aCmpSE2 := FWVetByDic(aCmpSE2, "SE2")

        aAutocab := {}

        For nX := 1 To Len(aCmpSE2)
            AAdd( aAutocab, aCmpSE2[nX] )
        Next nX

        For nX := 1 To Len(aCampoAUT)
            AAdd( aAutocab, aCampoAUT[nX] )
        Next nX

		//Alimentacao da variavel de data de contabilizacao (E2_EMIS1). O modulo de GPE pode gravar o campo E2_EMISSAO atraves
		//da rotina GPEM670 com data futura. Com isso o campo E2_EMIS1 fica inconsistente pois fica com a data base do
		//processamento, que eh inferior ao determinado no campo de emissao. Isso faz com que em rotinas como o FINR550 sejam
		//listados titulos em intervalos erroneos, por exemplo, um titulo com emissao no mes de dezembro saindo no intervalo
		//do mes de novembro.
		If lRet
			If (nT := aScan(aRotAuto,{|x| x[1] == "E2_ORIGEM"})) > 0
				If Substr(Upper(aRotAuto[nT][2]),1,3) $ "GPE/APT"
					lRAGPE := .T.
				Endif
			Endif
			If !lRAGPE
				lRAGPE := IIf(Upper(ProcName(1)) $ "GPE/APT",.T.,.F.)
			Endif
			If lRAGPE
				If (nT := aScan(aRotAuto,{|x| x[1] == "E2_EMISSAO"})) > 0
					If ValType(aRotAuto[nT][2]) == "D" .AND. !Empty(aRotAuto[nT][2])
						If aRotAuto[nT][2] > dDataBase
							dDataEmis1 := aRotAuto[nT][2]
						Endif
					Endif
				Endif
			Endif
		Endif
	Endif
	If lRet

		LoteCont( "FIN" )

		//Selecionar ordem 1 para Cadastro de Fornecedores
		SA2->(dbSetOrder(1))

		// Ponto de entrada para pre-validar os dados a serem  exibidos.
		IF lF050BROW
			ExecBlock("F050BROW",.f.,.f.)
		Endif

		// A funcao SomaAbat reabre o SE2 com outro nome pela ChkFile, pois o filtro do SE2, desconsidera os abatimentos							|
		SomaAbat("","","","P")

		//Inicializo variaveis para rateio
		Debito  	:= ""
		Credito 	:= ""
		CustoD		:= ""
		CustoC		:= ""
		ItemD 		:= ""
		ItemC 		:= ""
		CLVLD		:= ""
		CLVLC		:= ""
		Conta		:= ""
		Custo 		:= ""
		Historico 	:= ""
		ITEM		:= ""
		CLVL		:= ""

		Afill(aDadosRet,0)
	Endif

	If lF050Auto
        If lRet
            Default nOpcAuto := 3
            MBrowseAuto(nOpcAuto,aAutoCab,"SE2")
        EndIf
	Else
		If nOpcAuto<>Nil
			Do Case
				Case nOpcAuto == 3
					INCLUI := .T.
					ALTERA := .F.
				Case nOpcAuto == 4
					INCLUI := .F.
					ALTERA := .T.
				OtherWise
					INCLUI := .F.
					ALTERA := .F.
			EndCase
			// Chamada direta da funcao de Inclusao/Alteracao/Visualizacao/Exclusao
			If lPanelFin  //Chamado pelo Painel Financeiro
				nPos := nOpcAuto
			Else
				nPos := Ascan(aRotina,{|x| x[4]== nOpcAuto})
			Endif

			// Nao encontrou a opcao, verifica se eh Visualizacao do rateio
			If nOpcAuto == 8 // Visualizacao do rateio
				nPos := Ascan(aRotina,{|x| x[2]== "FA050Rateio" })
			Endif
			If ( nPos # 0 )
				bBlock := &( "{ |x,y,z,k| " + aRotina[ nPos,2 ] + "(x,y,z,k) }" )
				dbSelectArea("SE2")
				Eval( bBlock,Alias(),SE2->(Recno()),nPos)
			EndIf
		Else
			// Endereca a funcao de BROWSE
			IF bExecuta = NIL// AWR - AVERAGE - 11/08/2003
				mBrowse( 6, 1,22,75,"SE2",,,,,, Fa040Legenda("SE2"),,,,,,,,IIF(lF050FILB,ExecBlock("F050FILB",.f.,.f.),NIL))
			ELSE
				aAreaAVG := GetArea()
				dbSelectArea("SE2")
				EVAL(bExecuta)// AWR - AVERAGE - 11/08/2003
				RestArea(aAreaAVG)
			ENDIF
		EndIf
	EndIf
	// Recupera a Integridade dos dados
	Custo   := ""
	Valor   := 0
	Debito  := ""
	Credito := ""
	ItemD	:= ""
	ItemC   := ""
	__aVAAuto := Nil

    //Motor de retenção - Restaura variáveis static
    Clean050Mr()

    If __lLocBRA
        f050LRatIR(.T.)
    EndIf    

	Set Key VK_F12 To
	RestInter() // Restaura variaveis publicas

    CTB105MVC(lGet105MVC) // Restaura Static CTBA105:CTB105MVC():lRotMVC

Return

//-------------------------------------------------------------------
/*/{Protheus.doc}FA050Inclu
Programa p/ ìnclusão de Contas a Pagar
para empresas publicas.
@author Wagner Xavier
/*/
//-------------------------------------------------------------------
Function FA050Inclu(cAlias AS Character, nReg AS Numeric, nOpc AS Numeric, ;
                        cRec1 AS Character, cRec2 AS Character, lSubst AS Logical) AS Logical

    Local aAreaSubs  as Array
    Local aBut050    as Array
    Local aDim       as Array
    Local aEaiRet    as Array
    Local aFKFLoc    as Array
    Local aFKGLoc    as Array
    Local aParam     as Array
    Local aTituloCC  as Array
    Local bIRPFBaixa as Block
    Local cTudoOK    as Character
    Local lCalcIssBx as Logical
    Local lF050ADPC  as Logical
    Local lF050CAN   as Logical
    Local lF050INC   as Logical
    Local lFKF       as Logical
    Local lFKG       as Logical
    Local lPanelFin  as Logical
    Local lPCCBaixa  as Logical
    LOCAL lPodeInc   as Logical
    Local lRatPrj    as Logical
    Local lRet       as Logical
    Local nFim       as Numeric
    Local nIndexAtu  as Numeric
    Local nInicio    as Numeric
    LOCAL nOpca      as Numeric
    Local nPosAFR    as Numeric
    Local nPosEv     as Numeric
    Local nTcSql     as Numeric
    Local lFaClmFKF  as Logical
    Local lFaClmFKG  as Logical
    Local aF50Clm    as Array

    PRIVATE aAutoAfr    as Array
    Private aBordINSS   as Array
    Private aCposAlter  as Array
    Private aDadosIr    as Array
    PRIVATE aHeader     as Array
    PRIVATE aParcacre   as Array
    PRIVATE aParcDecre  as Array
    PRIVATE aParcelas   as Array
    PRIVATE aRatAFR     as Array
    Private aRecnoINSS  as Array
    Private aSE2FI2     as Array
    PRIVATE bPMSDlgFI   as Block
    PRIVATE cCarteira   as Character
    PRIVATE cHistDsd    as Character
    PRIVATE cModRetPIS  as Character
    Private cOldNaturez as Character
    Private cPretIns    as Character
    Private cSeqCv4     as Character
    PRIVATE cTitPaiAB   as Character
    Private dVencReaAnt as Date
    Private lRatOk      as Logical
    PRIVATE _Opc        as Numeric
    Private nBaseIns    as Numeric
    Private nBCalINS    as Numeric
    Private nBCalIRF    as Numeric
    Private nCofBaseC   as Numeric
    Private nCofBaseR   as Numeric
    Private nCofCalc    as Numeric
    Private nCslBaseC   as Numeric
    Private nCslBaseR   as Numeric
    Private nCslCalc    as Numeric
    Private nInss       as Numeric
    Private nIrfBaseR   as Numeric
    Private nPisBaseC   as Numeric
    Private nPisBaseR   as Numeric
    Private nPisCalc    as Numeric
    Private nSaveSx8Len as Numeric
    PRIVATE nUsado      as Numeric
    Private nVCalINS    as Numeric
    Private nVCalIRF    as Numeric
    PRIVATE nVlRetIrf   as Numeric
    Private nVretInss   as Numeric

    DEFAULT lSubst     := .F.
    Default __lIntPFS  := SuperGetMv("MV_JURXFIN",.T.,.F.) //Integração do Financeiro com o Juridico(Habilitado = .T.)
    Default __lTemMR   := (FindFunction("FTemMotor") .and. FTemMotor())
    Default __lMetric  := FwLibVersion() >= "20210517"
    Default __lFA50UPD := ExistBlock("FA050UPD")
    Default __lHasEAI  := FWHasEAI("FINA050", .T.,, .T.)

    lRet        := .T.
    lPanelFin   := IsPanelFin()
    nOpca	    := 0
    lPodeInc    := .T.
    nIndexAtu   := SE2->(IndexOrd())
    //Controla o Pis Cofins e Csll na baixa
    lPCCBaixa   := SuperGetMv("MV_BX10925",.T.,"2") == "1"
    // Controla IRPF na Baixa
    bIRPFBaixa  := {|| IIf( __lLocBRA, SA2->A2_CALCIRF == "2", .F.) }
    lCalcIssBx  := IsIssBx("P")
    aAreaSubs   := {}
    // Utilizado na AxInclui (Deve ter 4 linhas)
    // aParam[1] = Funcao executada antes da interface
    // aParam[2] = Funcao executada ao confirmar (TudoOk)
    // aParam[3] = Funcao executada dentro da transacao (AxInclui)
    // aParam[4] = Funcao executada apos a transacao
    aParam      := {{|| .T. }, ;
                    {|| lF050Auto .Or. IIF(F050VRAT(),Iif(MV_MULNATP .And.  M->E2_MULTNAT == "1", ;
                        MultNat2("SE2",3,Iif(mv_par06==1,;
                        (Iif(Eval(bIRPFBaixa) .And. !(M->E2_TIPO $ MVPAGANT),0,M->E2_IRRF)+;
                        Iif(!lCalcIssBx,M->E2_ISS,0)+;
                        M->E2_RETENC + M->E2_SEST +;
                        IIF(lPccBaixa,0,M->E2_PIS+M->E2_COFINS+M->E2_CSLL))+;
                        M->E2_INSS,0),(mv_par10==2 .And. mv_par06==2)),.T.),.F.)},;
                    {|| .T. },;
                    {|| .T. }} // Utilizado na AxInclui
    aDim 		:= {}
    aTituloCC   := {}
    lF050CAN	:= ExistBlock( "F050CAN" )
    lF050INC    := ExistBlock("FA050INC")
    lF050ADPC   := ExistBlock( "F050ADPC" )
    lRatPrj	    := .T. //indica se existe rateio de projetos
    nPosAFR	    := 0 //indica se existe rateio de projetos na autocab de titulos
    aEaiRet     := {}
    lFKG        := .F.
    aFKGLoc     := {}
    nPosEv      := 0
    lFKF        := .F.
    aFKFLoc     := {}
    nInicio     := 0
    nFim        := 0

    aHeader	    := {}
    nUsado 	    := 0
    cCarteira   := "P"
    cHistDsd	:= CRIAVAR("E2_HIST",.F.)  // Historico p/ Desdobramento
    aParcelas	:= {}  // Array para desdobramento
    aParcacre   := {}
    aParcDecre  := {}
    aRatAFR		:= {}
    aAutoAfr	:= {}//array automatico de rateio de projetos
    cModRetPIS  := GetNewPar( "MV_RT10925", "1" )
    cTitPaiAB   := RTrim(SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA))
    nVlRetIrf   := 0
    nVCalIRF    := 0
    nBCalIRF    := 0
    nCslCalc	:= 0
    nCslBaseC	:= 0
    nPisCalc	:= 0
    nPisBaseC	:= 0
    nCofCalc	:= 0
    nCofBaseC	:= 0
    nPisBaseR	:= 0
    nCofBaseR	:= 0
    nCslBaseR	:= 0
    nIrfBaseR	:= 0    
    nVCalINS    := 0
    nBCalINS    := 0
    aDadosIr    := {0,{},{}}
    nSaveSx8Len := GetSx8Len()
    cSeqCv4		:= ""
    _Opc 		:= nOpc
    aSE2FI2		:= {} // Utilizada para gravacao das justificativas
    aCposAlter  := {}
    dVencReaAnt	:= cTod('')	// Utilizado para avaliar alteração no vencimento real
    nBaseIns	:= 0 //Inss Baixa
    nVretInss	:= 0
    aRecnoINSS  := {}
    aBordINSS	:= {}
    cPretIns	:= ""
    nInss		:= 0
    lRatOk      := .T.
    lFaClmFKF	:= ExistBlock( "FACLMFKF" )
    lFaClmFKG   := ExistBlock( "FACLMFKG" )
    aF50Clm     := {}

    If nOpc == 3 // inclusao
        nSaveSx8 := GetSX8Len()
        cSeqCv4 := GetSxENum("CV4", "CV4_SEQUEN")
        WHILE .T.
            IF CV4->(!DbSeek(xFilial("CV4")+cSeqCv4))
                WHILE (GetSx8Len() > nSaveSx8)
                    ConfirmSX8()
                END
                EXIT
            ENDIF
            cSeqCv4 := GetSXENum("CV4", "CV4_SEQUEN")
        ENDDO
    EndIf

    __cFunBkp := FunName()
    __cFunMet := Iif(AllTrim(__cFunBkp)=='RPC',"RPCFINA050",__cFunBkp)

    If __lMetric
        SetFunName(__cFunMet)
        // Metrica de controle de acessos 
        FwCustomMetrics():setSumMetric(Alltrim(ProcName()), "financeiro-protheus_qtd-por-acesso_total", 1)
        SetFunName(__cFunBkp)
    Endif

    If (!Type("lF050Auto") == "L" .Or. !lF050Auto)
        __lRatDes := .F.
    EndIf

    //Integracao com SIGAPMS
    If IntePms()
        If lSubst
            aAreaSubs := GetArea()
            PmsDlgFS(4,"","","",,"","",.F.) // Carrega os valores no array sem chamar a GetDados
            bPMSDlgFI	:= {||PmsDlgFS(4,M->E2_PREFIXO,M->E2_NUM,M->E2_PARCELA,M->E2_TIPO,M->E2_FORNECE,M->E2_LOJA)}
            RestArea(aAreaSubs)
        Else
            bPMSDlgFI	:= {||PmsDlgFI(3,M->E2_PREFIXO,M->E2_NUM,M->E2_PARCELA,M->E2_TIPO,M->E2_FORNECE,M->E2_LOJA)}
        EndIf

        // integração com o PMS
        If IntePMS() .And. (!Type("lF050Auto") == "L" .Or. !lF050Auto)
            SetKey(VK_F10, {|| Eval(bPMSDlgFI)})
        EndIf
    Endif

    nOldValor 	:= 0
    nOldSaldo 	:= 0
    nOldISS		:= 0
    nOldBtrISS	:= 0
    nOldIRR		:= 0
    nOldInss	:= 0
    nOldSEST	:= 0
    nOldAcre    := 0
    nOldDecre   := 0
    nOldPis		:= 0
    nOldCofins	:= 0
    nOldCsll	:= 0
    nOldTxMoeda	:= 0
    __lRateioIR := .F.


    If !lF050Auto
        nVlRetPis := 0
        nVlRetCof := 0
        nVlRetCsl := 0
        aDadosRet := Array(5)
        Afill(aDadosRet,0)
    Endif

    lIntegracao := IF(GetMV("MV_EASYFIN")=="S",.T.,.F.)

    //Botoes adicionais na EnchoiceBar
    aBut050 := fa050BAR("IntePms()")

    If __lLocBRA .and. !lPccBaixa
        Aadd(aBut050,{"NOTE",{||F050CalcRt()},STR0125,STR0126})  //"Modalidade de Retenção Pis/Cofins/Csll"###"Impostos"
    EndIf

    //Motor de retenções
    If __lTemMR
        Aadd(aBut050,{"NOTE",{||F050MRET()},STR0301,STR0301})  //"'Retenção de Impostos'
    EndIf

    If __lLocBRA .and. FindFunction("FINMRATIR")
        Aadd(aBut050,{"RATEIOIR",{||FINMRATIR(If(__lRateioIR, __oRatIRF:aRatIRF, NIL))},STR0348,STR0348})  //"Rateio de IR progressivo"
    EndIf

    dbSelectArea( cAlias )
    dbSetOrder(1)

    lAltera:=.F.
    IF __lFA50UPD
        // Ponto de Entrada para Pre-Validacao de Inclusao
        lPodeInc := ExecBlock("FA050UPD",.f.,.f.)
    Endif
    
    If __lFa986Vld == NIL
	    __lFa986Vld := FindFunction( "F986Vld" )
    Endif

    cCadastro := STR0007 // "Contas a Pagar"

    If !lF050Auto
        cTudoOk := 'Iif(M->E2_TIPO$MVPAGANT,DtMovFin(m->E2_EMISSAO).and.F050VldPa().and.fa050Num(),fa050Num() .And. (M->E2_RATEIO=="N" .Or. FA050TudCT('+Str(nOpc,2)+',"511","FINA050"'+'))).And.PcoVldLan("000002",IIF(M->E2_TIPO$MVPAGANT,"02","01"),"FINA050")'
        cTudoOk += ' .And. IIF(Len(aSE2FI2)==0,Fa050JUST(),.T.)'
        cTudoOk += ' .And. If(M->E2_TEMDOCS == "1",CN062NecDocs(),.T.) ' //Documentos
        cTudoOk += ' .And. FA050VldAp(M->E2_CODAPRO,M->E2_MOEDA)'
    Else
        cTudoOk := 'Iif(M->E2_TIPO$MVPAGANT,DtMovFin(m->E2_EMISSAO),fa050Num()).and. Fa050Moed()'
        cTudoOk += '.and. IIF(M->E2_DESDOBR=="S",F050DSDOBR(),.T.)'
        cTudoOk += '.and. F050RatAut(cLote) .And. PcoVldLan("000002",IIF(M->E2_TIPO$MVPAGANT,"02","01"),"FINA050")'
        cTudoOk += '.And. Obrigatorio(aGets,aTela)'

        If  IntePMS() .and. (nPosAFR:=AScan(aAutocab,{|x|AllTrim(x[1])=="AUTRATAFR"})) >0 //rateio automatico de projetos
            aAutoAFR:=aClone(aAutoCab[nPosAFR][2])
            cTudoOk+=' .and. F050AutAFR('+Str(nOpc,2)+') '
        Endif
    Endif
    
    If __lLocBRA .And. __lFa986Vld .And. !((lFaClmFKF .Or. lFaClmFKG) .And. lF050Auto)
        cTudoOk += '.And. F986Vld("SE2")'
    EndIf
    
    IF lF050INC
        cTudoOK += ' .AND. ExecBlock("FA050INC",.f.,.f.)'
    Endif

    IF lF050ADPC .and. FunName() = "MATA121"
        cTudoOK += ' .AND. ExecBlock("F050ADPC",.f.,.f.,{aValores})'
    Endif

    If Type("cValidaOK") = "C" .AND. !EMPTY(cValidaOK)// Usado caso a bExecuta # NIL
        cTudoOK += cValidaOK		// AWR - AVERAGE - 11/08/2003
    Endif

    cTudoOK += '.And. FA050VLMV()'
    cTudoOK += '.And. F050BtrISS()'

    cTudoOK += '.And. Iif(M->E2_TIPO $ MVABATIM, FA050Tipo(.T.) .And. fa050valor(), .T.)'

    //Validação Banco, Agência e Conta
    cTudoOK += '.and. IIF(M->E2_TIPO $ MVPAGANT, CarregaSa6(cBancoAdt,cAgenciaAdt,cNumCon,.F.,,.T.), .T.)'
    
    // se for adiantamento, valida se o fornecedor e loja escolhido estao conforme pedido/documento
    If !lF050Auto
        If Type("aRecnoAdt") != "U" .and. (FunName() = "MATA121" .or. FunName() = "MATA103")
            cTudoOk += ' .And. F050VlAdFoLj()'
        Endif
    Endif

    cTudoOk += ' .AND. F50VldBCOF() '

    //Validação de caracteres especiais
    cTudoOk += ' .And. F050VlCpos()'
    cTudoOK += ' .And. F050VldVlr() '

    If FindFunction("JurValidCP") .And. __lIntPFS
        cTudoOK += ' .And. JurValidCP(3) '
    EndIf

    cTudoOK += ' .and. f050RatOk(lRatOK) .And. F986NatRen() '

    // Verifica se data do movimento n„o ‚ menor que data limite de
    // movimentacao no financeiro
    If !DtMovFin(,,"1")
        Return
    Endif

    // Inicializa a gravacao dos lancamentos do SIGAPCO
    PcoIniLan("000002")

    If ( lF050Auto )
    
        RegToMemory("SE2",.T.,.F.)

        If MV_MULNATP
            FINXTMP()
        EndIF

        nInicio := Seconds()
        If EnchAuto(cAlias,aAutoCab,cTudoOk,nOpc)

            nPosEv := AScan(aAutocab,{|x|AllTrim(x[1]) == "AUTCMTIT"})
            If nPosEv > 0
                aFKFLoc := aClone(aAutoCab[nPosEv][2])
                lFKF    := .T.
            EndIf

            nPosEv := AScan(aAutocab,{|x|AllTrim(x[1]) == "AUTCMIMP"})
            If nPosEv > 0
                aFKGLoc := aClone(aAutoCab[nPosEv][2])
                lFKG    := .T.
            EndIf

            If lFaClmFKF
                aF50Clm := ExecBlock("FACLMFKF",.F.,.F.,{aFKFLoc,"SE2",3})               
            If Valtype(aF50Clm) == "A"
                If !Empty(aF50Clm)
                    aFKFLoc := ACLONE(aF50Clm)
                    lFKF    := .T.
                EndIf 
            EndIf 
                aF50Clm := {}
            EndIf    

            If lFaClmFKG 
                aF50Clm := ExecBlock("FACLMFKG",.F.,.F.,{aFKGLoc,"SE2",3})
                If Valtype(aF50Clm) == "A"
                    If !Empty(aF50Clm)
                        aFKGLoc := ACLONE(aF50Clm)
                        lFKG    := .T.
                    EndIf   
                EndIf 
                aF50Clm := {}
            EndIf

            If cPaisLoc == "BRA" .and. (lFKF .or. lFKG)
                lRet := F986ExAut("SE2", aFKFLoc, aFKGLoc, 3, aAutocab)
            EndIf

            If FwIsInCallStack("GeraParcSe2")	//Desdobramento
                FA050Nat2()
            Endif
            nFim := Seconds() - nInicio
            If __lMetric
                SetFunName(__cFunMet)
                // Metrica do tempo das validações execauto
                FwCustomMetrics():setAverageMetric("AutTempoVld", "financeiro-protheus_tempo-conclusão-processo_seconds", nFim)
                SetFunName(__cFunBkp)
            Endif

            nInicio := Seconds()
            nOpca := AxIncluiAuto(cAlias,cTudoOk,"FA050AXINC('"+cAlias+"')" )

                If nOpca == 1 .and. __lMetric
                nFim := Seconds() - nInicio
                SetFunName(__cFunMet)
                // Metrica do tempo das validações execauto
                FwCustomMetrics():setAverageMetric("TempoGravação", "financeiro-protheus_tempo-conclusão-processo_seconds", nFim)
                SetFunName(__cFunBkp)
            Endif
        Else
            lMsErroAuto := .T.
            lRet := .F.
        EndIf       
    ElseIf lPodeInc
        nValDig := 0 //zera a variável para não trazer o vlr preenchido, após uma alteração.

        If lPanelFin  //Chamado pelo Painel Financeiro
            dbSelectArea("SE2")
            RegToMemory("SE2",.T.,,.F.,FunName())
            oPanelDados := FinWindow:GetVisPanel()
            oPanelDados:FreeChildren()
            aDim := DLGinPANEL(oPanelDados)
            nOpca := AxInclui(cAlias,nReg,nOpc,, "FA050INIS",,cTudoOk,,"FA050AXINC('"+cAlias+"')",aBut050,aParam,/*aAuto*/,/*lVirtual*/,/*lMaximized*/,/*cTela*/,.T.,oPanelDados,aDim,FinWindow)

        Else
            RegToMemory("SE2",.T.,,.F.,FunName())
            nOpca := AxInclui(cAlias,nReg,nOpc,, "FA050INIS", ,cTudoOk,,"FA050AXINC('"+cAlias+"')",aBut050,aParam,/*aAuto*/,/*lVirtual*/,/*lMaximized*/,/*cTela*/,/*lPanelFin*/,/*oFather*/,/*aDim*/,/*uArea*/,/*lFlat*/,lSubst)

            If nOpca <> 1
                Do While ( GetSx8Len() > nSaveSx8Len )
                    RollBackSX8()
                EndDo
            EndIf

            //Limpas as varivaéis para ser carregada na próxima inclusão.
            If !Empty(cBancoAdt)
                cBancoAdt := space(len(cBancoAdt))
                cAgenciaAdt := space(len(cAgenciaAdt))
                cChequeAdt := space(len(cChequeAdt))
                cHistor := space(len(cHistor))
                cBenef := space(len(cBenef))
                cNumCon := space(len(cNumCon))
            EndIf
        EndIf

        //Controle de Cartão de Credito para o Equador...
        If nOpca == 1 .and. cPaisLoc == "EQU" .and. SE2->E2_TIPO == "CC " .and. ProcName(1) <> "FA050TIT2CC"
            //Executar dialogo para obter os dados do Cartão de Crédito e gravar arquivo de controle FRC
            aTituloCC := Fa050GetCC(.T.)
            If Len(aTituloCC) > 0
                Fa050GrvFRC(aTituloCC)
            EndIf
        EndIf

        // Motor de retenção
        // Restaura variáveis static
        Clean050Mr()
        nValDig := 0 //zera a variável para não trazer o vlr preenchido na próxima inclusão.
    EndIf

    If nOpca <> 1
        lRet := .F.
    EndIf

    // Integração protheus X tin.
    If lRet .and. __lHasEAI
        lRatPrj := PmsRatPrj("SE2",,SE2->E2_PREFIXO,SE2->E2_NUM,SE2->E2_PARCELA,SE2->E2_TIPO,SE2->E2_FORNECE,SE2->E2_LOJA)
        If !( AllTrim(SE2->E2_TIPO) $ MVPAGANT .and. lRatPrj  .and. !(cPaisLoc $ "BRA|")) //nao integra PA  para Totvs Obras e Projetos Localizado
            aEaiRet := FWIntegDef('FINA050',,,, 'FINA050')
            If !aEaiRet[1]
                Help(" ", 1, "HELP", STR0315, STR0316 + CRLF + aEAIRET[2], 3, 1)  // "Erro EAI" / "Problemas na integração EAI. Transação não executada."
                lRet := .F.
                nOpcA := 2
            Endif
        Endif
    Endif

    // grava array para uso na rotina de adiantamento do pedido de compra/documento de entrada
    If nOpcA = 1 .and. Type("aRecnoAdt") != "U" .and. (FunName() = "MATA121" .or. FunName() = "MATA103")
        aAdd(aRecnoAdt,{SE2->(RECNO()),SE2->E2_VALOR})
    Endif
    
    // Finaliza a gravacao dos lancamentos do SIGAPCO
    If nOldValor != M->E2_VALOR .or. lF050Auto
        PcoFinLan("000002")
    Endif

    // Executa ponto de entrada para permitir customizar regra ao  cancelar a inclusao do titulo
    If lF050CAN .And. nOpca <> 1
        ExecBlock( "F050CAN", .F., .F. )
    EndIf

    // Verifica o arquivo de rateio, e deleta o conteúdo do arquivo temporario
    // para que no proximo rateio seja reutilizado a mesma tabela no banco
    If __lLocBRA .And. !__lRatDes
        If Select("TMP") > 0 .And. !Empty(__cFIN1Name)
            nTcSql := TcSQLExec("DELETE FROM "+__cFIN1Name)
            FChkTCExec(nTcSql, 1)
        EndIf
    EndIf

    If nOpcA = 1 .and. __lMetric
        SetFunName(__cFunMet)
		// Metrica de controle da origem 
        FwCustomMetrics():setSumMetric("E2_ORIGEM_"+Alltrim(SE2->E2_ORIGEM), "financeiro-protheus_qtd-por-conteudo_total", 1)
        // Metrica de controle da Moeda 
        FwCustomMetrics():setSumMetric("E2_MOEDA_"+AllTrim(cValToChar(SE2->E2_MOEDA)), "financeiro-protheus_qtd-por-conteudo_total", 1)
        SetFunName(__cFunBkp)
    Endif

    aDadosRet := Array(5)

    If IntePMS() .And. (!Type("lF050Auto") == "L" .Or. !lF050Auto)
        SetKey(VK_F10, Nil)
    EndIf
    If __lLocBRA
        F986LimpaVar() //Limpa as variaveis estaticas - Complemento de Titulo
        f050LRatIR(.T.)
    EndIf

    SE2->(dbSetOrder(nIndexAtu))

    If MV_MULNATP .and. select("SEZTMP") >  0
        FINXDETMP()
    EndIF

    If lF050Auto .And. lMsErroAuto
        AutoGrLog(STR0312)
    EndIf

    //Inicialização da variavel utilizada para controlar se o usuario selecionou o titulo pai do abatimento
    __SelPai := .F.

Return nOpca

//-------------------------------------------------------------------
/*/{Protheus.doc}FA050Delet
Programa p/ exclusão Contas a Pagar
para empresas publicas.
@author Wagner Xavier
@since  27/04/92
/*/
//-------------------------------------------------------------------
Function FA050Delet(cAlias AS Character, nReg AS Numeric, nOpc AS Numeric) AS Logical

    Local aArea        as Array
    Local aAreaAnt     as Array
    Local aAreaSA2     as Array
    Local aAreaSE2     as Array
    Local aBut050      as Array
    Local aChave       as Array
    Local aDiario      as Array
    Local aEaiRet      as Array
    Local aExcSE3      as Array
    Local aFlagCTB     as Array
    Local aPenCont     as Array
    Local aTitImp      as Array
    Local aTpImp       as Array
    Local cAliasFor    as Character
    Local cArquivo     as Character
    Local cBusca       as Character
    Local cChaveCV4    as Character
    Local cChaveFK7    as Character
    Local cChaveTit    as Character
    Local cFornece     as Character
    Local cLoja        as Character
    Local cMVISS       as Character
    Local cNatImp      as Character
    Local cNatureza    as Character
    Local cNum         as Character
    Local cPadMon      as Character
    Local cPadrao      as Character
    Local cParcCIDE    as Character
    Local cParcCof     as Character
    Local cParcCsll    as Character
    Local cParcela     as Character
    Local cParcINP     as Character
    LOCAL cParcIr      as Character
    LOCAL cParcIss     as Character
    Local cParcPis     as Character
    Local cPrefixo     as Character
    Local cQryFor      as Character
    Local cQryVend     as Character
    Local cSEST        as Character
    Local cTipImp      as Character
    Local cTipo        as Character
    Local cTipoSE2     as Character
    Local cTitPai      as Character
    Local cUltima      as Character
    Local lAchou       as Logical
    Local lAtuForn     as Logical
    Local lAtuSldNat   as Logical
	Local lCIDE        as Logical // Define o fato gerador do imposto CIDE. 1 = Baixa ou 2 = Emissão
    Local lComisExc    as Logical
    Local lCpRet       as Logical
    Local lCtMovPa     as Logical // Indica se a Contabilizacao do LP513/LP514  ocorrer pelo Titulo(SE2) ou Mov.Bancario(SE5) do Pagamento Antecipado. 1="SE2" / 2="SE5"
    Local lDelGPE      as Logical
    Local lDelTit      as Logical
    Local lDesdobr     as Logical
    Local lDigita      as Logical
    Local lDistrato    as Logical //Variavel usada pelo Template GEM
    Local lEstProv     as Logical //Variavel para estornar título provisório
    Local lF050DEL1    as Logical
    Local lF050INS     as Logical
    Local lFA050B01    as Logical
    Local lFA050Del    as Logical
    Local lFA050RAT    as Logical
    Local lGPEExcTit   as Logical // Define se podera excluir titulo gerado pelo SIGAGPE no SIGAFIN
    Local lHead        as Logical
    Local lIdenLA      as Logical
    Local lImpOld      as Logical
    Local lIRPFBaixa   as Logical
    Local lOk          as Logical // Retorno do ExecBlock( FA050Del )
    Local lPadrao      as Logical
    Local lPanelFin    as Logical
    Local lPCCBaixa    as Logical
    Local lRastro      as Logical
    Local lRateioPCO   as Logical
    Local lRatPrj      as Logical //indica se existe rateio de projetos
    Local lRet         as Logical
    Local lRetQry      as Logical
    Local lSetAuto     as Logical
    Local lSetHelp     as Logical
    Local lTemCheq     as Logical
    Local lUsaFlag     as Logical
    Local lViaAFR      as Logical
    Local lViaINT      as Logical
    Local nCIDE        as Numeric
    Local nCofins      as Numeric
    Local nCsll        as Numeric
    Local nHdlPrv      as Numeric
    Local nI           as Numeric
    Local nIndex       as Numeric
    Local nMoedSE2     as Numeric
    Local nOpcA        as Numeric
    Local nOrd         as Numeric
    Local nOrdSE2      as Numeric
    Local nPis         as Numeric
    Local nProxReg     as Numeric
    Local nRecnoFJA    as Numeric
    Local nRecnoSE2    as Numeric
    Local nRecSef      as Numeric
    Local nRegAtu      as Numeric
    Local nSavRec      as Numeric
    Local nTotal       as Numeric
    Local nValINSSPatr as Numeric
    Local nValSaldo    as Numeric
    Local nVretCof     as Numeric
    Local nVretCsl     as Numeric
    Local nVretPis     as Numeric
    Local nX           as Numeric
    Local oDlg         as Object
    Local oModel       as Object
    Local oSubFKA      as Object

    //Exclusao chamada a partir do cancelamento de desdobramento
    Local lFina250      AS Logical
    Local lFina590      AS Logical
    Local lFina379      AS Logical
    Local lFindTemp     AS Logical
    Local lPass         AS Logical
    Local lExistFJU     AS Logical
        
    Local lAdComPart    AS Logical
    Local cFilFIE       AS Character

    Local aLimpaSE3     AS Array
    Local nY            AS Numeric

    Local cNaturINSS    AS Character
    Local cNaturPIS     AS Character
    Local cNaturCOF     AS Character
    Local cNaturCSLL    AS Character
    Local cNaturIRF     AS Character
    Local nTcSql        AS Numeric

    PRIVATE _Opc        AS Numeric
    PRIVATE aHeader     AS Array
    PRIVATE aRatAFR		AS Array
    PRIVATE bPMSDlgFI	AS Block
    PRIVATE lTitRetD 	AS Logical	//	Indica se o titulo retentor poderá ser Deletado
    PRIVATE nTitRetD 	AS Numeric 	// 	Recno do titulo retentor que será Deletado
    PRIVATE nUsado      AS Numeric

    Default __lIntPFS  := SuperGetMv("MV_JURXFIN",.T.,.F.) //Integração do Financeiro com o Juridico(Habilitado = .T.)
    Default __lNRasDSD := SuperGetMV("MV_NRASDSD",.T.,.F.)
    Default __lMetric  := FwLibVersion() >= "20210517"
    Default __lPmsInt  := IsIntegTop(,.T.)
    Default __lPLSFN50 := FindFunction("PLSFN050")
    Default __lTemMR   := (FindFunction("FTemMotor") .and. FTemMotor())
    Default __lHasEAI  := FWHasEAI("FINA050", .T.,, .T.)
    Default __lFA50UPD := ExistBlock("FA050UPD")

    nOpcA       := 0
    nSavRec     := 0
    nRecSef     := 0
    nTotal	    := 0
    nHdlPrv	    := 0
    nIndex 	    := IndexOrd()
    nValSaldo   := 0
    nI          := 0
    nX          := 0
    nOrd        := 0
    nRegAtu     := 0
    nProxReg    := SE2->(Recno())
    nPis		:= 0
    nCofins	    := 0
    nCsll		:= 0
    nVretPis    := 0
    nVretCof    := 0
    nVretCsl    := 0
    nRecnoFJA   := 0
    nValINSSPatr:= 0
    nRecnoSE2   := 0
    cPrefixo    := ""
    cNum        := ""
    cParcela    := ""
    cTipo       := ""
    cFornece    := ""
    cLoja       := ""
    cNatureza   := ""
    cBusca      := ""
    cTitPai     := SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)
    cPadrao     := ""
    cArquivo    := ""
    cSEST       := GetMv("MV_SEST",,"")
    nOrdSE2     := 0
    nMoedSE2    := SE2->E2_MOEDA
    cTipoSE2    := SE2->E2_TIPO
    cParcIr     := ""
    cParcIss    := ""
    cParcPis    := ""
    cParcCof    := ""
    cParcCsll   := ""
    cPadMon     := "59B" //Contabilizacao do estorno da varia monetaria
    cChaveCV4   := ""
    cQryVend    := ""
    cQryFor     := ""
    cAliasFor   := ""
    cUltima     := ""
    cParcINP    := " "
    cChaveTit   := ""
    cChaveFK7   := ""
    cNatImp     := ""
    cTipImp     := ""
    cMVISS      := GetMv("MV_ISS")
    lPanelFin   := IsPanelFin()
    lTemCheq 	:= .F.
    lPadrao     := .F.
    lOk         := .T.    // Retorno do ExecBlock( FA050Del )
    lHead       := .F.
    lDesdobr    := .F.
    lDistrato   := .F. //Variavel usada pelo Template GEM
    oDlg        := NIL
    oModel      := NiL
    oSubFKA     := Nil
    aBut050     := {}
    aDiario     := {}
    aTitImp     := {}
    aArea       := {}
    aTpImp      := {}
    aFlagCTB    := {}
    aAreaSE2    := {}
    aAreaSA2    := {}
    aChave	    := {}
    aPenCont	:= {}
    aEaiRet     := {}
    aAreaAnt    := {}
    aExcSE3     := {CtoD(""),""}
    lIRPFBaixa  := .F.
    lRateioPCO  := .F.
    lDelTit     := .T.
    lSetAuto    := .F.
    lSetHelp    := .F.
    lDelGPE     := .F.
    lAchou      := .F.
    lAtuSldNat  := .T.
    lViaAFR     := .T.
    lViaINT     := .F.
    lEstProv    := .F.   //Variavel para estornar título provisório
    lRatPrj	    := .T. //indica se existe rateio de projetos
    lCpRet      := .F.
    lDigita     := .F.
    lRetQry     := .T.
    lImpOld     := .F.
    lRet        := .T.
    lRastro	    := FVerRstFin()
    lF050INS	:= ExistBlock("F050INS")
    lF050DEL1   := ExistBlock("F050DEL1")
    lFA050Del   := ExistBlock("FA050Del")
    lFA050B01   := ExistBlock("FA050B01")
    lFA050RAT   := ExistBlock("FA050RAT")
    lComisExc   := ExistBlock("F050DSE3")
    lGPEExcTit  := GetMV( "MV_GPEEXTT", , .T. )	// Define se podera excluir titulo gerado pelo SIGAGPE no SIGAFIN
    lUsaFlag	:= SuperGetMV( "MV_CTBFLAG" , .T. /*lHelp*/, .F. /*cPadrao*/)
    lPCCBaixa   := SuperGetMv("MV_BX10925",.T.,"2") == "1"
    lAtuForn    := SuperGetMv("MV_ATUFORN",.F.,.T.)
    lCIDE 		:= cPaisLoc == "BRA" .And. SuperGetMv("MV_FGCIDE",.T.,"2") == "2" // Define o fato gerador do imposto CIDE. 1 = Baixa ou 2 = Emissão
    nCIDE       := 0
    cParcCIDE   := ""
    lCtMovPa    := SuperGetMv("MV_CTMOVPA",.T.,"1") == "2" // Indica se a Contabilizacao do LP513/LP514  ocorrer pelo Titulo(SE2) ou Mov.Bancario(SE5) do Pagamento Antecipado. 1="SE2" / 2="SE5"
    lIdenLA     := .F. 
    lFina250    := FwIsInCallStack("FACANDSD")
    lFina590    := FwIsInCallStack("FINA590")
    lFina379    := FwIsInCallStack("FINA379")
    lFindTemp   := FindFunction("T_AE_EXCSE2")
    lPass       := FwIsInCallStack("T_AE_ExcSe2")
    lExistFJU   := FJU->(ColumnPos("FJU_RECPAI")) >0 .and. FindFunction("FinGrvEx")
    lAdComPart  := .F.
    cFilFIE     := ""
    aLimpaSE3   := {}
    nY          := 1

    cNaturINSS  := GetMv("MV_INSS")
    cNaturPIS   := GetMv("MV_PISNAT")
    cNaturCOF   := GetMv("MV_COFINS")
    cNaturCSLL  := GetMv("MV_CSLL")
    cNaturIRF   := GetMv("MV_IRF")

    aHeader     := {}
    nUsado      := 0
    aRatAFR		:= {}
    bPMSDlgFI	:= {||PmsDlgFI(2,M->E2_PREFIXO,M->E2_NUM,M->E2_PARCELA,M->E2_TIPO,M->E2_FORNECE,M->E2_LOJA)}
    _Opc        := nOpc
    lTitRetD 	:= .F. 		//	Indica se o titulo retentor poderá ser Deletado
    nTitRetD 	:= RecNo() 	// 	Recno do titulo retentor que será Deletado

    SomaAbat("","","","P")

    nSavRec	  := RecNo()
    cPrefixo  := E2_PREFIXO
    cNum	  := E2_NUM
    cParcela  := E2_PARCELA
    cTipo 	  := E2_TIPO
    cFornece  := E2_FORNECE
    cLoja	  := E2_LOJA
    cNatureza := E2_NATUREZ
    cParcIr	  := E2_PARCIR
    cParcIss  := E2_PARCISS
    cParcInss := E2_PARCINS
    cParcSEST := E2_PARCSES
    nIss	  := SE2->E2_ISS
    nInss	  := SE2->E2_INSS
    nSEST	  := E2_SEST

    __cFunBkp := FunName()
    __cFunMet := Iif(AllTrim(__cFunBkp)=='RPC',"RPCFINA050",__cFunBkp)

    If __lMetric
        SetFunName(__cFunMet)
        // Metrica de controle de acessos 
        FwCustomMetrics():setSumMetric(Alltrim(ProcName()), "financeiro-protheus_qtd-por-acesso_total", 1)
        SetFunName(__cFunBkp)
    Endif

    If __lLocBRA
        cParcCIDE := SE2->E2_PARCCID
        nCIDE	  := SE2->E2_CIDE
    EndIf

    lF050Auto := IF(Type("lF050Auto") == "U", .F., lF050Auto)

    __lRateioIR := .F.

    SA2->(dbSetOrder(1))
    SA2->(MSSeek(xFilial("SA2")+SE2->(E2_FORNECE+E2_LOJA)))
    lIRProg := IIf(__lLocBRA,IIf(!Empty(SA2->A2_IRPROG),SA2->A2_IRPROG,"2"),"2")
    lIRPFBaixa := IIf( __lLocBRA, SA2->A2_CALCIRF == "2", .F.)

    nPis		 := SE2->E2_PIS
    nCofins	     := SE2->E2_COFINS
    nCsll		 := SE2->E2_CSLL
    cParcPis	 := SE2->E2_PARCPIS
    cParcCof	 := SE2->E2_PARCCOF
    cParcCsll    := SE2->E2_PARCSLL
    nVretPis	 := SE2->E2_VRETPIS
    nVretCof	 := SE2->E2_VRETCOF
    nVretCsl	 := SE2->E2_VRETCSL

    
    If __lGesplan == Nil    
        __lGesplan  := SuperGetMv("MV_FINTGES",.F.,.F.) .And. FindFunction("FUpdStamp")
    EndIF

    lIntegracao := IF(GetMV("MV_EASYFIN")=="S",.T.,.F.)
    lVerifyBlq  := Iif(Type("lVerifyBlq") == "U",.T.,lVerifyBlq)

    //Botoes adicionais na EnchoiceBar
    aBut050 := fa050BAR('SE2->E2_PROJPMS == "1"')

    //inclusao do botao Posicao
    AADD(aBut050, {"HISTORIC", {|| Fc050Con() }, STR0204}) //"Posicao"

    //inclusao do botao Rastreamento
    AADD(aBut050, {"HISTORIC", {|| Fin250Pag(2) }, STR0205}) //"Rastreamento"

    // integração com o PMS
    If IntePMS() .And. SE2->E2_PROJPMS == "1"
        SetKey(VK_F10, {|| Eval(bPMSDlgFI)})
    EndIf

    // Não excluir um Titulo que veio da Integração com o TOP - Wilson em 15/08/2011
    If __lPmsInt .And. SE2->E2_ORIGEM # "WSFINA05"
        If !FwIsInCallStack("FINI050")//o adapter pode excluir o titulo
            aArea     := GetArea()
            aAreaAFR  := AFR->(GetArea())
            aAreaSCP  := SCP->(GetArea())
            dbSelectArea("AFR")
            dbSetOrder(2)
            If MsSeek(xFilial("AFR")+SE2->E2_PREFIXO+SE2->E2_NUM+SE2->E2_PARCELA+SE2->E2_TIPO+SE2->E2_FORNECE+SE2->E2_LOJA)
                If lViaAFR
                    lViaINT := IIf(AFR->AFR_VIAINT == 'S',.T.,.F.)
                    If lViaINT
                        Help(,,"INTPMS",,STR0203,1,0) // "Titulo Integrado pelo TOP só pode ser excluido pelo TOP"
                        MsUnLockAll()
                        Return .F.
                    End
                End
            End
            RestArea(aAreaSCP)
            RestArea(aAreaAFR)
            RestArea(aArea)
        Endif
    End

    // Validação do documento hábil - SIAFI
    If FinTemDH()
        Return .T.
    Endif

    // AAF - Titulos originados no SIGAEFF não devem ser alterados
    If !lF050Auto .AND. "SIGAEFF" $ SE2->E2_ORIGEM
        Help(" ",1,"FAORIEFF")
        Return
    EndIf

    //DFS - 16/03/11 - Deve-se verificar se os títulos foram gerados por módulos Trade-Easy, antes de apresentar a mensagem.
    // TDF - 26/12/11 - Acrescentado o módulo EFF para permitir liquidação
    // NCF - 25/03/13 - Acrescentado o módulo SIGAESS (Siscoserv)

    If lIntegracao .and. (UPPER(Alltrim(SE2->E2_ORIGEM)) $ "SIGAEEC/SIGAEDC/SIGAECO/SIGAESS" .OR.( SE2->E2_PREFIXO == 'EIC'.AND. UPPER(Alltrim(SE2->E2_ORIGEM))$'SIGAEIC' ) ) .AND. !(cModulo $ "EEC/EIC/EDC/ECO/EFF/ESS")
        HELP(" ",1,"FAORIEEC")
        Return
    Endif

    // Verifica se o titulo foi gerado pela rotina de distrato do Template GEM
    If HasTemplate("LOT") .AND. ExistTemplate("GEMSE2DIS")
        If lDistrato := ExecTemplate("GEMSE2DIS",.F.,.F.)
            MsgAlert("Este título foi gerado pela rotina de distrato do template GEM, portanto nao poderá ser excluído.")
            Return .F.
        EndIf
    EndIf

    If AllTrim(SE2->E2_ORIGEM) == "JURCTORC" .And. (!FindFunction("JurIsRest") .Or. !JurIsRest()) // Integração Controle Orçamentário SIGAPFS x SIGAFIN
        HELP(" ", 1, "FAORIPFS") // "Este título foi gerado pela rotina de Controle Orçamentário - SIGAPFS, portanto não poderá ser excluído."
        Return .F.
    EndIf

    //Verificar se o documento foi ajustado por diferencia de cambio.
    If cPaisLoc $ "ARG|ANG|COL|MEX|URU"
        SIX->(DbSetOrder(1))
        If SIX->(MsSeek('SFR'))
            SFR->(DbSetOrder(1))
            If SFR->(MsSeek(xFilial()+"2"+SE2->E2_PREFIXO+SE2->E2_NUM+SE2->E2_PARCELA+SE2->E2_TIPO+SE2->E2_FORNECE+SE2->E2_LOJA))
                Help( " ", 1, "FA084010",,Left(SFR->FR_CHAVDE,Len(SE2->E2_PREFIXO+SE2->E2_NUM+SE2->E2_PARCELA+SE2->E2_TIPO)),5)
                Return .F.
            Endif
        Endif
    Endif

    If cPaisLoc == "RUS"
        If R604Is48(SE2->E2_FILIAL,SE2->E2_PREFIXO,SE2->E2_NUM,SE2->E2_PARCELA,SE2->E2_TIPO,SE2->E2_FORNECE,SE2->E2_LOJA)
            Help(" ",1,STR0303)
            Return
        EndiF
    EndIf

    // Caso tenha seja um INV, gerado pelo SigaEic e do Brasil nao podera se excluido
    If lIntegracao .and.  cPaisLoc <> "ARG"   .and. SE2->E2_Tipo = "INV" .and. !(cModulo $ UPPER(SE2->E2_Origem)) .and. !lF050Auto
        HELP(" ",1,"FAORIEIC")
        Return .F.
    Endif

    // Caso tenha seja um PR, gerado pelo SigaEic  nao podera ser excluido
    If lIntegracao .and. SE2->E2_Tipo = "PR" .and. UPPER(SE2->E2_Origem) = "SIGAEIC"
        HELP(" ",1,"FAORIEIC")
        Return .F.
    Endif

    //verifica se e titulo originado do SIGAPLS e nao deixa excluir.
    if __lPLSFN50 .and. ! lF050Auto .and. PLSFN050(nOpc)
        Return(.f.)
    EndIf

    If  AllTrim(SubStr(SE2->E2_ORIGEM, 1, 8)) $ 'MATA460A' .And. !lF050Auto
        Help(" ",1,"NO_DELETE",,SE2->E2_ORIGEM,3,1) //Este titulo nao podera ser excluido pois foi gerado pelo modulo
        Return .F.
    EndIf

    // Não deixa fazer a exclusão dos títulos gerados pelas rotinas de
    // Aglutinação de Impostos.
    // Não deixa excluir titulos de origem FINA667 - Adtos. de viagens
    // Não deixa excluir titulos de origem FINA686 - Conferencia de servicos II
    If AllTrim(SE2->E2_ORIGEM) $ 'FINA376#FINA378#FINA374#FINA667#FINA677#FINA685#FINA686#FINA381' .And. !lF050Auto
        Help(" ",1,"NO_DELETE",,SE2->E2_ORIGEM,3,1) //Este titulo nao podera ser excluido pois foi gerado pelo modulo
        Return .F.
    EndIf

    // Integracao com o Modulo de Transporte (SIGATMS)
    If  AllTrim(SE2->E2_ORIGEM) $ 'SIGATMS|TOTVSGFE' .And. !lF050Auto
        Help(" ",1,"FA050TMS",,SE2->E2_ORIGEM,4,1) //Este titulo nao podera ser excluido pois foi gerado pelo modulo
        Return .F.
    EndIf

    // Se for um PA ou um cheque gerado por um PA deverá cancelar a Ordem de Pago.
    If cPaisLoc $ "ARG|ANG|MEX|COL"
        If (SE2->E2_TIPO=="PA " .And.!Empty(SE2->E2_ORDPAGO)).Or.(SE2->E2_TIPO == "CH ".And.!Empty(SE2->E2_ORDPAGO))
            Help(" ",1,"OrdPago")
            Return .F.
        Endif
    Endif

    // usa o Modulo 88 GTP
    If nModulo <> 88
        If  Upper(substr(SE2->E2_ORIGEM,1,7)) $ IIF(FindFunction('GTPFUNCRET'),GTPFUNCRET('FINA050','3','SE2'),'GTPA421|GTPA700|GTPA700A|GTPA700L|GTPA819')
            Help(" ",1,"NODELGTP",,STR0321,1,0) //"Este título não pode ser excluido ou cancelada sua baixa ,pois foi gerado através do GTP."
            Return
        EndIf
    EndIf

    // Verifica se o titulo esta bloqueado - Gestao de Contratos
    If !Empty(SE2->(FieldPos("E2_MSBLQL"))) .And. SE2->E2_MSBLQL == "1" .And. lVerifyBlq
        Help(" ",1,"SE2BLOQ")
        Return .F.
    EndIf

    // Verifica se o titulo esta em DARF
    If __lLocBRA
        If AllTrim(SE2->E2_IDDARF) <> ""
            Help(" ",1,"SE2DARF1") //Este titulo nao podera ser excluido pois faz parte de uma DARF.
            Return .F.
        EndIf
    EndIf

    // Verifica se data do movimento n„o ‚ menor que data limite de
    // movimentacao no financeiro
    If !DtMovFin(,,"1")
        Return
    Endif

    // Verifica se titulo foi conciliado por DDA
    If !Empty(SE2->E2_CODBAR)
        If VldConcDda(SE2->E2_FILIAL, SE2->E2_FORNECE, SE2->E2_LOJA, SE2->E2_CODBAR, SE2->E2_FILIAL+ "|" + SE2->E2_PREFIXO+"|" + SE2->E2_NUM+"|" +;
        SE2->E2_PARCELA+"|" + SE2->E2_TIPO+"|" + SE2->E2_FORNECE+"|" + SE2->E2_LOJA + "|")
            Help('',1,'FIN050DDA',,STR0273,1,0)
            Return
        EndIf
    EndIf

    // Caso seja uma parcela gerada pela rotina de aplicação/emprestimo não podera ser excluida
    If Alltrim(SE2->E2_ORIGEM) == "FINA171"
        Help(" ",1,"NO_DELETE",,STR0280,3,1) //"Empr‚stimo com gera‡?o de parcelas (FINA171)"
        Return .F.
    Endif

    If F050VTitEmp()
        If !(MSGYESNO(STR0281+CHR(10)+CHR(13)+STR0282,STR0026))	//"Este t¡tulo possui caracter¡sticas semelhantes a um titulo gerado pelo processo de empr‚stimo."###"Deseja realmente excluir este t¡tulo?"
            Return .F.
        Endif
    Endif

    If FindFunction("JurValidCP") .And. __lIntPFS
        If !JurValidCP(5)
            Return .F.
        EndIf
    EndIf

    // Verifica movimentacao de AVP
    If !FAVPValTit( "SE2",, SE2->E2_PREFIXO, SE2->E2_NUM, SE2->E2_PARCELA, SE2->E2_TIPO, SE2->E2_FORNECE, SE2->E2_LOJA, " ", lF050Auto )
        Return .F.
    EndIf

    //Permito deletar titulo de impostos gerado pelo modulo financeiro e que nao possuir titulo pai
    If SE2->E2_TIPO == MVTAXA .And. "FINA" $ Upper(SE2->E2_ORIGEM) .And. !Fa050Pai()
        lDelTit := .F.
    Else
        lDelTit := .T.
    EndIf

    // Verifica se os dados nao foram gravados por outro modulo
    cE2Origem := Upper(Trim(SE2->E2_ORIGEM))

    If !Empty(cE2Origem) .And. !cE2Origem $ "FINA050|FINA181|SIGATMS|TOTVSGFE|TAFA444" .And. !"GPE" $ cE2Origem .And. !"APT" $ cE2Origem .And.;
        !lF050Auto .And. cModulo <> "EIC" .And. lDelTit .And. !(cE2Origem == "MATA460A" .And. SE2->E2_PREFIXO == "ICM") .And.;
        nModulo <> 17 .And. !Fa50Vendor() .And. !(cPaisloc == "BOL" .And. cE2Origem == "FISA032" .And. SE2->E2_SALDO == SE2->E2_VALOR)
        If cE2Origem == "WSFINA05"
            MsgAlert(STR0203) // "Titulo Integrado pelo TOP so pode ser excluido pelo TOP"
            Return .F.
        Else
            Help(" ",1,"NO_DELETE2")
            Return .F.
        EndIf

    ElseIF  (Alltrim(SE2->E2_NATUREZ)) == "VENDOR" .and. Empty(SE2->E2_TITORIG) .and. cE2Origem $ "FINA090"
        cTeste:=  SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE)
        cAliasVend	:=	GetNextAlias()
        cQryVend := "SELECT * "
        cQryVend += "FROM "+RetSqlName("SE2")+" WHERE "
        cQryVend += "E2_TITORIG='"+cTeste+"' AND "
        cQryVend += "E2_BAIXA IS NOT NULL AND "
        cQryVend += "D_E_L_E_T_=' ' "
        cQryVend := ChangeQuery(cQryVend)

        dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQryVend),cAliasVend,.F.,.T.)
        If (cAliasVend)->(!Eof())
            Help(" ",1,"F050VEND")
            lRetQry := .F.
        ENDIF
        (cAliasVend)->(dbCloseArea())
        If !lRetQry
            Return .F.
        Endif
    ELSE
        If Empty(cE2Origem) .and. SE2->E2_TIPO == "TX " .and. ;
        (Alltrim(SE2->E2_NATUREZ) == Alltrim(SuperGetMv("MV_ICMS",.F.,"ICMS")) .or. ;
        Alltrim(SE2->E2_NATUREZ) == Alltrim(SuperGetMv("MV_IPI",.F.,"IPI"))  .or. ;
        Alltrim(SE2->E2_NATUREZ) $ "ICMS|IPI" )
            Help(" ",1,"NO_DELETE2")
            Return
        Endif
    EndIf

    //Titulo de impostos(PCC) gerado pelo modulo financeiro e que possui um titulo pai
    If !lFina590 .And. lDelTit
        cNatImp := AllTrim(GetMv("MV_PISNAT"))+"|"+AllTrim(GetMv("MV_COFINS"))+"|"+AllTrim(GetMv("MV_CSLL"))
        cTipImp := PADR(SE2->E2_FILIAL+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA, TamSx3("FK7_CHAVE")[1])
        lImpOld := SE2->E2_TIPO $ MVTAXA .And. Alltrim(SE2->E2_NATUREZ) $ cNatImp

        If (__lTemMR .And. !FinTitImp("SE2", SE2->E2_TIPO, "P", cTipImp)) .Or. lImpOld
            If lImpOld
                Help(" ",1,"NODELETA",, STR0157 , 4,0) //"Titulo de Impostos Pis, Cofins ou Csll, Altere o Titulo Pai"
            Else
                Help(" ",1,"NODELETA",, STR0302 , 4,0)  //"Títulos de impostos não podem ser excluídos. É necessário excluir o Título Pai!"
            EndIf
            Return .F.
        EndIf
    EndIf

    //Permito exclusão de titulo gerado pela folha
    If "GPE" $ cE2Origem .And. !lF050Auto
        If lGPEExcTit	// Define se podera excluir titulo gerado pelo SIGAGPE no SIGAFIN
            If ! (MSGYESNO(STR0120+CHR(10)+CHR(13)+STR0121,STR0026))		//"Este titulo foi gerado pelo modulo SIGAGPE - Gestao de Pessoal."###"Deseja realmente deleta-lo ?"###"Atencao"
                Return
            Else
                // Controla exclusão do título na tabela RC1 (Gestão de Pessoal).
                lDelGPE := .T.
            EndIf
        Else
            MsgAlert( STR0120 + CHR(10) + CHR(13) + STR0206, STR0026 )		//STR0206 - "A exclusão somente pode ser realizada no módulo SIGAGPE."
            Return
        Endif
    EndIf

    //Permito exclusão de titulo gerado pelo Processo Trabalhista.
    If "APT" $ cE2Origem .And. !lF050Auto
        If !(MSGYESNO(STR0148+CHR(10)+CHR(13)+STR0149,STR0026))		//"Este titulo foi gerado pelo modulo SIGAAPT - Processo Trabalhista."###"Deseja realmente deleta-lo ?"###"Atencao"
            Return
        Endif
    EndIf

    // Verifica se o titulo nao esta em bordero
    If !Empty(SE2->E2_NUMBOR)
        Help("",1,"FA050BORD")
        Return  .F.
    Else
        // Caso seja o titulo principal, verifica se existe titulo de impostos
        // gerado, e confirma se estes estao ou nao em um outro bordero.
        aTitImp := ImpCtaPg()
        For nX := 1 To Len(aTitImp)

            If !Empty(aTitImp[nX][8]) .and. (aTitImp[nX][7] == aTitImp[nX][6])
                Help("",1,"FA050BORD")
                Return  .F.
            Endif
        Next
    EndIf

    If cPaisLoc == "ARG"
        //Verifica si el título tiene preorden de pago
        If (!Empty(SE2->E2_PREOP) .And. !lFina250)
            Help(" ", 1, "PREORDPAGO", , STR0367, 2, 0,,,,,, {STR0368})
            Return .F.
        EndIf
    EndIf
    
    If !Empty(SE2->E2_BAIXA) .and. !lFina250
        Help(" ",1,"FA050BAIXA")
        Return .F.
    EndIf

    If SE2->E2_VALOR != SE2->E2_SALDO .and. !lFina250
        Help(" ",1,"BAIXAPARC")
        Return .F.
    EndIf

    If lFindTemp .And. aliasIndic("LHP") .And. __lLocBRA .And. Iif (!lPass, !T_AE_EXCSE2(.F.,SE2->(Recno())), .f.)
        Help(,,"TEMPCDV",,STR0244,1,0) // "Título gerado pelo Template CDV só pode ser excluído pela rotina de origem."
        Return .F.
    EndIf
    //O Titulo Principal deste Imposto foi baixado
    If __lLocBRA .And. (SE2->E2_TIPO $ MVTAXA+"/"+MVINSS+"/"+MVISS+"/"+MVTXA+"/"+"SES"+"/"+"INA") .And. F050BxPai() .AND. !lFina590
        Help(" ",1,"FA050BAIXA")
        Return .F.
    EndIf

    // Verifica se nao ‚ um titulo de ISS ou IR ou INSS ou SEST ou CIDE
    IF __lLocBRA .And. SE2->E2_TIPO $ MVISS+"/"+MVTAXA+"/"+MVTXA+"/"+MVINSS+"/"+"SES"+"/"+"CID"+"/"+"INA" .And. !lFina379 // Recalculo do PCC (FINA379)
        If Fa050Pai()
            Help(" ",1,"NOVALORIR")
            Return .F.
        EndIf
    EndIf

    //Verifica a possibilidade de Alteração de um titulo que teve seus impostos(PCC)
    //Retidos em outro Titulo(Retentor)
    //Este procedimento NAO sera efetuado na seguinte situacao (exemplo)
    //- Foi realizado um desdobramento com calculo de impostos na emissao (PCC)
    //- Foram geradas 3 parcelas de 2.000 e a retencao do PCC foi na terceira parcela
    //- Foi incluido um outro titulo (simples) de 2.000 com calculo de PCC
    //- Ao se cancelar o desdobramento, sobraria apenas o titulo de 2.000 e nao deveria haver retencao do PCC
    //- Por utilizar algo em torno de 4 rotinas atumaticas encadeadas, o sistema apresenta mensagens inconsistentes
    //  e nao permite o cancelamento do desdobramento.
    If (SE2->E2_PRETPIS	= '2' .Or. SE2->E2_PRETCOF	= '2' .Or. SE2->E2_PRETCSL	= '2') .and. !lFina250
        If	F050VerAlt()
            lTitRetD := .T.
        Else
            Return .F.
        EndIf
    Endif
    If !lPCCBaixa
        lCpRet:= SLDRMSG(SE2->E2_EMISSAO, SE2->E2_SALDO,SE2->E2_NATUREZ,"P",SE2->E2_FORNECE,SE2->E2_LOJA,SE2->E2_TIPO)
        If lCpRet
            If !IsBlind() .AND. !MSGNoYes(STR0249) // "Essa baixa possui impostos retidos em outra baixa, deseja continuar ?"
                Return
            Endif
        Endif
    Endif

    // Validação dos títulos filhos
    If ! SE2->E2_TIPO $ MVISS+"/"+MVTAXA+"/"+MVTXA+"/"+MVINSS+"/"+"SES"+"/"+"CID"+"/"+"INA"+"/"+"INP"
        If !Fa050Filho(.T.) // Verifica se um dos titulos de impostos já foi baixado e nao permite a exclusao
            If !lF050DEL1 .Or. !ExecBlock("F050DEL1", .F., .F.) // Ponto de entrada permitira a exclusao
                Help(" ",1,"NODELETA",,STR0131, 4, 0) // "Este titulo possui impostos e"+chr(13)+"um desses impostos sofreu baixa"
                Return .F.
            Endif
        ElseIf !Fa050FDarf(.F.) // Verifica se tem titulo de imposto que esteja em DARF.
            Help(" ",1,"SE2DARF2") // "Este titulo nao podera ser excluido""pois gerou títulos de impostos que""fazem parte de uma DARF."
            Return .F.
        Endif
    Endif

    // Verifica se foi emitido cheque para este titulo
    IF SE2->E2_IMPCHEQ == "S"
        Help( " ", 1, "EXISTCHEQ" )
        Return( .F. )
    END

    // Verifica se foi emitido cheque para um dos titulos de impostos	 -  Verifica na delecao do titulo pai
    If Fa050VerImp()
        Help( " ", 1, "EXISTCHEQ" )
        Return( .F. )
    EndIf

    //Verifica se existe tratamento de rastreamento
    //Verifica se o titulo foi gerador ou gerado por desdobramento
    dbSelectArea("FI8")
    dbSetOrder(2)	// FI8_FILIAL+FI8_PRFDES+FI8_NUMDES+FI8_PARDES+FI8_TIPDES+FI8_FORDES+FI8_LOJDES
    lAchou:= MsSeek(xFilial("FI8")+SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO))

    If lRastro .AND. SE2->E2_DESDOBR $ "1#S" .AND. lAchou .AND. !__lNRasDSD .and. !lFina250
        Help( " ", 1, "DESDOBRAD",,STR0152+Chr(13)+;  //"Não é possivel a exclusão de titutos geradores ou gerados por desdobramento. "
        STR0153,1)	//"Favor utilizar a rotina de Cancelamento de Desdobramento."
        Return .F.
    Endif

    // Verifica se adiantamento tem relacionamento com pedido de compra.
    If cPaisLoc $ "BRA|MEX" .and. SE2->E2_TIPO $ MVPAGANT
    	lAdComPart := FWSIXUtil():ExistIndex( 'FIE', '5' ) .and.  "C" $ (FwModeAccess('FIE',1) + FwModeAccess('FIE',2)  + FwModeAccess('FIE',3))
        //Avalia se é compartilhado ou exclusivo
        If lAdComPart
            cFilFIE := SE2->E2_FILORIG
            FIE->(dbSetOrder(5))    // Ajusta o indice para usar o FILORI
        Else
            cFilFIE := xFilial("FIE",SE2->E2_FILORIG)
            FIE->(dbSetOrder(3))
        Endif

        If FIE->(MsSeek(cFilFIE + "P" + SE2->(E2_FORNECE+E2_LOJA+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO)))
            Help(" ",,"NODELETE_PA",,STR0167,1,0,,,,,, {STR0373}) //"Não é permitida a exclusão de título de adiantamento relacionado a um pedido de compra."###"Exclua o relacionamento do adiantamento junto ao pedido de compras para que seja permitida a exclusão."
            Return .F.
        Endif
    Endif
    If SE2->E2_TIPO $ MVPAGANT .and. SE2->E2_EMISSAO > dDataBase
        dbSelectArea("SE5")
        dbSetOrder(7)
        If MsSeek(xFilial("SE5")+SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA))
            Help( " ", 1, "NODELPA" )
            Return .F.
        EndIf
    EndIf

    IF __lFA50UPD
        // Ponto de Entrada para Pre-Validacao de Exclusao
        IF !ExecBlock("FA050UPD",.f.,.f.)
            Return .F.
        Endif
    Endif

    //Verifica se o titulo foi originado de uma substituição de provisórios
    dbselectarea("FII")
    dbsetorder(2)
    If dbseek(xFilial("FII")+"SE2"+SE2->E2_PREFIXO+SE2->E2_NUM+SE2->E2_PARCELA+SE2->E2_TIPO+SE2->E2_FORNECE+SE2->E2_LOJA)
        If (!Type("lF050Auto") == "L" .or. !lF050Auto)
            If MsgYesNo(STR0208,STR0115) //"Titulo Efetivo originado de Título(s) Provisório(s), deseja excluir o Efetivo e retornar o(s) Provisório(s) para o Status 'Em aberto'?")
                lEstProv := .T.
            else
                Return .F.
            endif
        Else
            lEstProv := .T.
        EndIf
    EndIf

    PRIVATE aTELA[0][0],aGETS[0]

    nOpcA := 2

    //Posiciona no fornecedor e natureza
    SA2->( dbSeek(xFilial("SA2")+SE2->E2_FORNECE+SE2->E2_LOJA) )
    SED->( dbSeek(xFilial("SED")+SE2->E2_NATUREZ) )

    If !SoftLock( "SE2" )
        Return  .F.
    EndIf

    // Se for um PA e houver uma Solicitação de Fundos que gerou este PA
    If __lLocBRA .and. SE2->E2_TIPO $ MVPAGANT
        FJA->(dbSetOrder(6))
        If FJA->(dbSeek(xFilial("FJA")+cPrefixo+cNum+cParcela+cTipo+cFornece+cLoja ))
            nRecnoFJA := (FJA->(Recno()))
        Endif
    Endif

    dbSelectArea(cAlias)
    dbSetOrder(1)

    bCampo := {|nCPO| Field(nCPO) }
    FOR nI := 1 TO FCount()
        M->&(EVAL(bCampo,nI)) := FieldGet(nI)
    NEXT nI

    //Apresenta tela com dados do título
    If !Type("lF050Auto") == "L" .or. !lF050Auto
        If lPanelFin  //Chamado pelo Painel Financeiro
            dbSelectArea("SE2")
            RegToMemory("SE2",.F.,.F.,,FunName())
            oPanelDados := FinWindow:GetVisPanel()
            oPanelDados:FreeChildren()
            aDim := DLGinPANEL(oPanelDados)

            DEFINE MSDIALOG oDlg OF oPanelDados:oWnd FROM 0, 0 TO 0, 0 PIXEL STYLE nOR( WS_VISIBLE, WS_POPUP )

            aPosEnch := {,,,}
            oEnc01:= MsMGet():New( cAlias, nReg, nOpc,,"AC",STR0008,,aPosEnch,,,,,,oDlg,,,.F.) // "Quanto … exclus„o?"
            oEnc01:oBox:Align := CONTROL_ALIGN_ALLCLIENT

            // define dimenção da dialog
            oDlg:nWidth := aDim[4]-aDim[2]

            ACTIVATE MSDIALOG oDlg  ON INIT (FaMyBar(oDlg,{|| nOpca := 1,oDlg:End()},{|| nOpca := 2,oDlg:End()},aBut050,),	oDlg:Move(aDim[1],aDim[2],aDim[4]-aDim[2], aDim[3]-aDim[1]))

            FinVisual(cAlias,FinWindow,(cAlias)->(Recno()))

        Else
            nOpca := AxVisual(cAlias,nReg,2,,,,,aBut050)
        Endif
    Else
        nOpcA := 1
    EndIf

    If nOpcA == 1
        // Integração com o SigaPfs
        If __lIntPFS
            If !F050AtuPFS(5, nSavRec)
                Return .F.
            EndIf
        EndIf

        Begin Transaction

            //Deleta os conhecimentos/documentos relacionados ao titulo
            MsDocument( "SE2", SE2->( RecNo() ), 2, , 3 )

            SE5->(DbSelectarea("SE5"))
            SE5->(DbSetorder(7))
            If SE5->( DbSeek( xFilial("SE5")+SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)) )
                cChave := xFilial("SE5")+SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)
                While cChave == SE5->(E5_FILIAL + E5_PREFIXO + E5_NUMERO + E5_PARCELA + E5_TIPO + E5_CLIFOR + E5_LOJA )
                    If SE5->E5_TIPODOC $ "VM" .and. SE5->E5_RECPAG == 'P'

                        // Gera o lancamento contabil para delecao da varicao monetaria
                        cPadMon :=	cPadrao
                        cPadrao := "59B"
                        lPadrao := VerPadrao(cPadrao)

                        If lPadrao
                            If !lHead
                                nHdlPrv:=HeadProva(cLote,"FINA050",Substr(cUsuario,7,6),@cArquivo)
                                lHead := .T.
                            Endif
                            nTotal+=DetProva(nHdlPrv,cPadrao,"FINA050",cLote)

                            // Indica se a tela sera aberta para digitação
                            lDigita := IIf(mv_par01 == 1, .T., .F.)
                            cA100Incl(cArquivo,nHdlPrv,3,cLote,lDigita,.F.,,,,,,aDiario)
                        Endif

                        //Posiciona a FK5 para mandar a operação de alteração com base no registro posicionado da SE5
                        If AllTrim( SE5->E5_TABORI ) $ "FK2|FK6"

                            cModel := IIF(SE5->E5_TABORI == "FK6", 'FINM350','FINM020' )
                            oModel := FWLoadModel(cModel)
                            oModel:SetOperation( MODEL_OPERATION_UPDATE) //Alteração
                            oModel:Activate()
                            oSubFKA := oModel:GetModel( "FKADETAIL" )

                            If oSubFKA:SeekLine( { {"FKA_IDORIG", SE5->E5_IDORIG } } )

                                oModel:SetValue( "MASTER", "HISTMOV", STR0363 ) 
                                oModel:SetValue( "MASTER", "E5_GRV", .T. ) //Habilita gravação SE5
                                oModel:SetValue( "MASTER", "E5_OPERACAO", 3 ) //E5_OPERACAO 3 = Deleta da SE5 e sem gerar estorno na FK5
                                If cModel == "FINM350"
                                    oModel:SetValue( "MASTER", "CARTEIRA", "P" ) //Carteira do movimento
							    Endif
                                
                                If oModel:VldData()
                                    oModel:CommitData()
                                    lRet := .T.
                                Else
                                    lRet := .F.
                                    cLog := cValToChar(oModel:GetErrorMessage()[4]) + ' - '
                                    cLog += cValToChar(oModel:GetErrorMessage()[5]) + ' - '
                                    cLog += cValToChar(oModel:GetErrorMessage()[6])

                                    If lF050Auto
                                        Help( ,,"M050VALID",,cLog, 1, 0 )
                                    EndIf
                                EndIf
                            Endif
                            oModel:DeActivate()
                            oModel:Destroy()
                            oModel := NIL

                            If !lRet
                                DisarmTransaction()
                                Break
                            Endif

                            cPadrao:= cPadMon

                        EndIf
                    EndIf
                    SE5->(DbSkip())
                EndDo
            EndIf
            // Inicializa a gravacao dos lancamentos do SIGAPCO
            PcoIniLan("000002")

            // PE para verificacao se o titulo pode ser excluido ou nao.
            // Se retornar .T. continua o processo de exclusao, se .F. retorna
            // sem excluir o titulo.
            If lFA050Del
                lOk := ExecBlock("FA050Del",.F.,.F.)
            Endif

            aChave := {SE2->E2_FILIAL,SE2->E2_PREFIXO,SE2->E2_NUM,SE2->E2_PARCELA,SE2->E2_TIPO,SE2->E2_FORNECE,SE2->E2_LOJA}

            If lOk
                //Verifica se o titulo tem baixas com contabilização pendente
                aPenCont := FA050PenC(aChave)
                If Len(aPenCont) > 0 
                    lOk := FA050MonP(aPenCont)
                EndIf
            EndIf    

            If !lOk
                MsUnLock()
                DisarmTransaction()
                lRet := .F.
                Break
            Endif

            // Verifica se o titulo foi gerado por desdobramento.
            If SE2->E2_DESDOBR == "S"
                lDesdobr := .T.
            Endif

            // Verifica se o titulo foi distribuido por multiplas naturezas para contabilizar o
            // cancelamento via SE2 ou SEV
            If SE2->E2_MULTNAT == "1"
                If SEV->(MsSeek(RetChaveSev("SE2")))
                    // Vai para o final para nao contabilizar duas vezes o LP 515
                    SEV->(DbGoBottom())
                    SEV->(DbSkip())
                Endif
            Endif

            // Verifica se o titulo PA pode ser baixado
            If SE2->E2_TIPO $ MVPAGANT
                If !Fa050DelPa(.T., @lTemCheq, @nRecSef)
                    DisarmTransaction()
                    lRet := .F.
                    Break
                Endif
            EndIf

            //Inicia processo do lancamento no Pco quando possui rateio
            If SE2->E2_RATEIO=="S" .And. !Empty(SE2->E2_ARQRAT)
                PcoIniLan("000021")
                lRateioPCO := .T.
            EndIf

            // Integração protheus X tin.
            If __lHasEAI
                lRatPRj := PMSRatPrj("SE2",,SE2->E2_PREFIXO,SE2->E2_NUM,SE2->E2_PARCELA,SE2->E2_TIPO,SE2->E2_FORNECE,SE2->E2_LOJA)
                If !( AllTrim(SE2->E2_TIPO) $ MVPAGANT .and. lRatPrj  .and. !(cPaisLoc $ "BRA|")) //nao integra PA  para Totvs Obras e Projetos Localizado
                    aEaiRet := FWIntegDef('FINA050',,,, 'FINA050')
                    If !aEaiRet[1]
                        Help(" ", 1, "HELP", STR0315, STR0316 + CRLF + aEAIRET[2], 3, 1)  // "Erro EAI" / "Problemas na integração EAI. Transação não executada."
                        DisarmTransaction()
                        nOpcA := 2
                        lRet := .F.
                        Break
                    Endif
                Endif
            Endif

            If SE2->E2_RATEIO == "S"
                F050ExcTmp(cPadrao,lDesdobr) //Força criação da FwTemporaryTable para armazenar dados do rateio. O Begin Tran abaixo impossibilita inserir-la através do CTBRATFIN;
            Endif

            //PE para tratamentos complementares antes da exclusão e contabilização
            If lFA050B01
                ExecBlock("FA050B01",.F.,.F.)
            EndIf

            dbSelectArea("SE2")

            //Variavel utilizada na Integração com RM Solum, vai permitir passar
            //na Exclusão de itens do RM Solum apenas uma vez.
            lPrimeiro:=.T.
            // Atualizacao dos dados do Modulo SIGAPMS
            If IntePMS()
                PmsWriteFI(2,"SE2")	//Estorno
                PmsWriteFI(3,"SE2")	//Exclusao
            Endif
            // Apaga os lancamentos nas contas orcamentarias SIGAPCO
            If SE2->E2_TIPO $ MVPAGANT
                PcoDetLan("000002","02","FINA050",.T.)
            Else
                PcoDetLan("000002","01","FINA050",.T.)
            EndIf

            If SE2->E2_TEMDOCS == "1"
                CN062ApagDocs()
            EndIf

            IF !(E2_TIPO $ MVPROVIS) .or. mv_par02 == 1
                // Posiciona no registro referente ao Fornecedor
                SA2->(dbSeek(xFilial("SA2")+ SE2->E2_FORNECE + SE2->E2_LOJA))

                dbSelectArea("SE2")
                cPadrao := Iif(SE2->E2_RATEIO == "S","512","515")

                IF SE2->E2_TIPO $ MVPAGANT
                    If (cPaisLoc == "RUS" .AND. ALLTRIM(SE2->E2_ORIGEM) =="RU06D07")
                        cPadrao:=""
                    Else
                        cPadrao:="514"
                    Endif
                Endif

                //Exclui os Tx's de IRPJ
                If lIRPFBaixa
                    aAreaSA2 := SA2->(GetArea())
                    F241DelTxIR("FINA050",SE2->( Recno() ), SE2->E2_IRRF)
                    RestArea(aAreaSA2)
                EndIf

                // Verifica se titulos foram gerados via desdobramento
                // e altera o lancamento padrao para 578.
                If lDesdobr
                    cPadrao := "578"
                Endif

                lPadrao:=VerPadrao(cPadrao)

                //verificacao SIGAPLS
                IF __lPLSFN50 .And. lPadrao .And. "PLS" $ SE2->E2_ORIGEM
                    lPadrao := !PLSFN050(Nil, .F.)
                EndIf

                //realiza a exclus?o da tabela complementar
                If __lLocBRA .AND. !lDesdobr
                    Fa986excl("SE2")
                EndIf

                // Motor de Retenção
                // Ajusto as tabelas FK3/FK4/FK0 caso o titulo tenha retenção
                If __lTemMR .and. !(SE2->E2_TIPO $ MVPAGANT)
                    aAreaAnt := GetArea()
                    //Dados da tabela auxiliar com o código do título a pagar
                    cChaveTit := xFilial("SE2") + "|" + SE2->E2_PREFIXO + "|" + SE2->E2_NUM     + "|" + SE2->E2_PARCELA + "|" + ;
                                                        SE2->E2_TIPO    + "|" + SE2->E2_FORNECE + "|" + SE2->E2_LOJA
                    cChaveFK7 := FINBuscaFK7(cChaveTit, "SE2")

                    FMRDelImp( "SE2", cChaveFK7, 2 )
                EndIf

                // Deleta os titulos de Desdobramento em aberto
                If lDesdobr
                    // Apaga os lancamentos de desdobramento - SIGAPCO
                    PcoDetLan("000002","03","FINA050",.T.)

                    nValSaldo := 0
                    VALOR := 0
                    lHead := .F.
                    dDtEmiss := SE2->E2_EMISSAO
                    nMoedSE2 := SE2->E2_MOEDA
                    nOrdSE2 := IndexOrd()
                    // Gera o lancamento contabil para delecao de titulos
                    // gerados via desdobramento.
                    IF lPadrao .and. SubStr(SE2->E2_LA,1,1) == "S"  .and. __lNRasDSD
                        If !lHead
                            // Inicializa Lancamento Contabil
                            nHdlPrv := HeadProva( cLote,;
                            "FINA050" /*cPrograma*/,;
                            Substr(cUsuario,7,6),;
                            @cArquivo )
                            lHead := .T.
                        Endif
                        // Prepara Lancamento Contabil
                        //Contabiliza pela variavel VALOR. Nao necessita de controle de flag.
                        nTotal += DetProva( nHdlPrv,;
                        cPadrao,;
                        "FINA050" /*cPrograma*/,;
                        cLote,;
                        /*nLinha*/,;
                        /*lExecuta*/,;
                        /*cCriterio*/,;
                        /*lRateio*/,;
                        /*cChaveBusca*/,;
                        /*aCT5*/,;
                        /*lPosiciona*/,;
                        /*@aFlagCTB*/,;
                        /*aTabRecOri*/,;
                        /*aDadosProva*/ )
                        nValSaldo += SE2->E2_VALOR
                    Endif

                    nRegAtu := nRecnoSE2 := SE2->(Recno())
                    SE2->(dbSkip())
                    nProxReg := SE2->(Recno())
                    SE2->(dbGoto(nRegAtu))

                    If UsaSeqCor()
                        aDiario := {}
                        aDiario := {{"SE2",SE2->(recno()),SE2->E2_DIACTB,"E2_NODIA","E2_DIACTB"}}
                    Else
                        aDiario := {}
                    EndIf

                    If lAtuSldNat .And. SE2->E2_FLUXO == 'S'
                        lAchou := .F.
                        FI8->(DbSetOrder(1))
                        // Se nao for o titulo gerador do desdobramento, atualiza o saldo, pois o titulo gerador nao atualiza o saldo
                        // na inclusao
                        lAchou := FI8->(MsSeek(xFilial("FI8")+SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)))
                        If !lAchou
                            AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, If(SE2->E2_TIPO $ MVPAGANT+"/"+MV_CPNEG,"3","2"), "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, If(SE2->E2_TIPO $ MVABATIM, "+", "-"),,FunName(),"SE2",SE2->(Recno()),nOpc)
                        Endif
                    Endif

                    //Dados da tabela auxiliar com o código do título a pagar
                    cChaveTit := xFilial("SE2") + "|" + SE2->E2_PREFIXO + "|" + SE2->E2_NUM     + "|" + SE2->E2_PARCELA + "|" + ;
                                                        SE2->E2_TIPO    + "|" + SE2->E2_FORNECE + "|" + SE2->E2_LOJA


                    //realiza a exclusao da tabela complementar
                    If __lLocBRA
                        Fa986Excl("SE2")
                    EndIf

                    FINDELFKs(cChaveTit,"SE2")

                    RecLock("SE2",.F.,.T.)
                    dbDelete()
                    MsUnLock()

                    If nTotal > 0
                        dbSelectArea ("SE2")
                        dbGoBottom( )
                        dbSkip( )
                        VALOR := nValSaldo
                        // Prepara Lancamento Contabil
                        //Contabiliza pela variavel VALOR. Nao necessita de controle de flag.
                        nTotal += DetProva( nHdlPrv,;
                        cPadrao,;
                        "FINA050" /*cPrograma*/,;
                        cLote,;
                        /*nLinha*/,;
                        /*lExecuta*/,;
                        /*cCriterio*/,;
                        /*lRateio*/,;
                        /*cChaveBusca*/,;
                        /*aCT5*/,;
                        /*lPosiciona*/,;
                        /*@aFlagCTB*/,;
                        /*aTabRecOri*/,;
                        /*aDadosProva*/ )
                    Endif

                    IF lPadrao .and. nTotal > 0
                        //-- Se for rotina automatica força exibir mensagens na tela, pois mesmo quando não exibe os lançametnos, a tela
                        //-- sera exibida caso ocorram erros nos lançamentos padronizados
                        If lF050Auto
                            lSetAuto := _SetAutoMode(.F.)
                            lSetHelp := HelpInDark(.F.)
                            If Type('lMSHelpAuto') == 'L'
                                lMSHelpAuto := !lMSHelpAuto
                            EndIf
                        EndIf

                        // Envia para Lan‡amento Contábil - desdobramentos
                        cA100Incl( cArquivo,;
                        nHdlPrv,;
                        3 /*nOpcx*/,;
                        cLote,;
                        ( mv_par01 == 1 ) /*lDigita*/,;
                        ( mv_par07 == 1 ) /*lAglut*/,;
                        /*cOnLine*/,;
                        /*dData*/,;
                        /*dReproc*/,;
                        /*@aFlagCTB*/,;
                        /*aDadosProva*/,;
                        aDiario )
                        aFlagCTB := {}  // Limpa o coteudo apos a efetivacao do lancamento

                        SE2->(dbSetOrder(nOrdSE2))
                        If lF050Auto
                            HelpInDark(lSetHelp)
                            _SetAutoMode(lSetAuto)
                            If Type('lMSHelpAuto') == 'L'
                                lMSHelpAuto := !lMSHelpAuto
                            EndIf
                        EndIf
                    EndIf
                Else
                    dDtEmiss := SE2->E2_EMISSAO
                    nValSaldo := SE2->E2_VALOR
                    nMoedSE2 := SE2->E2_MOEDA
                EndIf
                
                If SE2->E2_TIPO $ MVPAGANT
                    If lCtMovPa .And. AllTrim(SE5->E5_LA) == "S"
                        lIdenLA := .T.
                    ElseIf !lCtMovPa .And. AllTrim(SE2->E2_LA) == "S"
                        lIdenLA := .T.
                    EndIf
                Else
                    lIdenLA := AllTrim(SE2->E2_LA) == "S"
                Endif

                // Monta contabilização - exceto desdobramentos
                IF lPadrao .and. lIdenLA .and. !lDesdobr
                    If cPadrao == "512"
                        CtbRatFin(cPadrao,"FINA050",cLote,3," ",nOpc)
                    Else
                        // Inicializa Lancamento Contabil
                        nHdlPrv := HeadProva( cLote,;
                        "FINA050" /*cPrograma*/,;
                        Substr(cUsuario,7,6),;
                        @cArquivo )
                        // Prepara Lancamento Contabil
                        If lUsaFlag  // Armazena em aFlagCTB para atualizar no modulo Contabil
                            aAdd( aFlagCTB, {"E2_LA", "S", "SE2", SE2->( Recno() ), 0, 0, 0} )
                        Endif
                        nTotal += DetProva( nHdlPrv,;
                        cPadrao,;
                        "FINA050" /*cPrograma*/,;
                        cLote,;
                        /*nLinha*/,;
                        /*lExecuta*/,;
                        /*cCriterio*/,;
                        /*lRateio*/,;
                        /*cChaveBusca*/,;
                        /*aCT5*/,;
                        /*lPosiciona*/,;
                        @aFlagCTB,;
                        /*aTabRecOri*/,;
                        /*aDadosProva*/ )
                    EndIf

                    If cPadrao $ "512|511"
                        If lFA050RAT
                            ExecBlock ("FA050RAT",.f.,.f.)
                        EndIf
                    EndIf
                EndIf

                // Busca moeda na qual se faz o controle de saldos em
                // moeda forte. A contabilizacao altera o valor da va-
                // riavel NMOEDA para 5, independente da moeda na qual
                // se faz esse controle.
                nMoeda 	 := Int(Val(GetMv("MV_MCUSTO")))
                If lDesdobr
                    SE2->(dbGoTo(nRegAtu))
                Else
                    SE2->(dbGoTo(nSavRec))
                Endif
                If !(cNatureza $ &(GetMv("MV_IRF"))) .And. lAtuForn
                    // Atualiza saldo do fornecedor
                    SA2->(DbSeek(XFILIAL("SA2")+SE2->E2_FORNECE+SE2->E2_LOJA))
                    SA2->(RecLock("SA2"))
                    If !(SE2->E2_TIPO $ MVPAGANT+"/"+MV_CPNEG+"/"+MVABATIM)
                        SA2->A2_SALDUP -= Round(NoRound(xMoeda(nValSaldo,nMoedSE2,1,dDtEmiss,3),3),2)
                        SA2->A2_SALDUPM-= Round(NoRound(xMoeda(nValSaldo,nMoedSE2,nMoeda,dDtEmiss,3),3),2)
                    Else
                        SA2->A2_SALDUP += Round(NoRound(xMoeda(SE2->E2_SALDO,SE2->E2_MOEDA,1,SE2->E2_EMISSAO,3),3),2)
                        SA2->A2_SALDUPM+= Round(NoRound(xMoeda(SE2->E2_SALDO,SE2->E2_MOEDA,nMoeda,SE2->E2_EMISSAO,3),3),2)
                    EndIf
                    MsUnlock()
                EndIf
            EndIf

            // Busca se tem comissao pagas para esse titulo, e tendo volta status para nao pago
            If __lLocBRA
                SE3->(dbSetOrder(4))    //E3_FILIAL + E3_PROCCOM
                cBusca := Left(xFilial("SE3") + SE2->( E2_PREFIXO + E2_NUM + E2_PARCELA ) + Space(Len(SE3->E3_PROCCOM)),Len(SE3->E3_PROCCOM))
                If SE3->(dbSeek(xFilial("SE3") + cBusca,.T.))
                    While SE3->(!Eof()) .And. xFilial("SE3") + cBusca == SE3->(E3_FILIAL + E3_PROCCOM)
                        If lComisExc
                            aExcSE3 := ExecBlock("F050DSE3",.F.,.F.,{SE3->(Recno())})
                            If Len(aExcSE3) >= 2
                                If ValType(aExcSE3[1]) # "D"
                                    aExcSE3[1] := Ctod("")
                                Endif
                                If ValType(aExcSE3[2]) # "C"
                                    aExcSE3[1] := ""
                                Endif
                            Else
                                aExcSE3 := {Ctod(""),""}
                            Endif
                        Endif
                        AADD( aLimpaSE3, {SE3->(RECNO() ), aExcSE3})
                        SE3->(dbSkip())
                    EndDo

                    For nY := 1 to Len(aLimpaSE3)
                        SE3->(DBGOTO(aLimpaSE3[nY,1]))
                        RecLock("SE3",.F.)
                        SE3->E3_DATA := aLimpaSE3[nY,2,1] //aExcSE3[1]
                        SE3->E3_PROCCOM := aLimpaSE3[nY,2,2] //aExcSE3[2]
                        SE3->(MsUnlock())
                    Next nY
                    dbSelectArea("SA2")
                Endif
            Endif

            // Faz tratamento do titulos de pagamento antecipado.
            If !(SE2->E2_TIPO $ MVPAGANT)
                dbSelectArea("SEF")
                dbSetOrder(7)
                If dbSeek(xFilial("SEF")+"P"+SE2->(E2_PREFIXO + E2_NUM + E2_PARCELA + E2_TIPO))
                    While !Eof() .and. xFilial("SEF") == SEF->EF_FILIAL .and. ;
                        SEF->(EF_PREFIXO + EF_TITULO + EF_PARCELA + EF_TIPO) == ;
                        SE2->(E2_PREFIXO + E2_NUM + E2_PARCELA + E2_TIPO)

                        nAtuRec := SE5->(RECNO())
                        dbSkip()
                        nProxRec := SE5->(Recno())
                        dbGoto(nAtuRec)

                        If SEF->(EF_FORNECE+EF_LOJA) == SE2->(E2_FORNECE+E2_LOJA)
                            RecLock("SEF")
                            Replace EF_KEY with EF_PREFIXO+EF_TITULO+EF_PARCELA+EF_TIPO+EF_FORNECE+EF_LOJA
                            Replace EF_PREFIXO With ""
                            Replace EF_TITULO With ""
                            Replace EF_PARCELA With ""
                            Replace EF_TIPO With ""
                            MsUnlock()
                            FKCOMMIT()
                        Endif
                        dbGoto(nProxRec)
                    Enddo
                Endif
            Else
                If !Fa050DelPa(.F., lTemCheq , nRecSef)
                    DisarmTransaction()
                    lRet := .F.
                    Break
                EndIf
            Endif

            // Exclui os registros do FRC - Tabela de Controle de Cartão de Credito.
            If cPaisLoc == "EQU" .and. AllTrim(SE2->E2_TIPO) == "CC" .and. Subs(ProcName(1),1,8) <> "FA099GRV"
                Fa050DelFRC()
            EndIf

            F050GrvSE5(2,.F.)

            If lExistFJU
                FinGrvEx("P")
            Endif
       
            // Apaga  o registro (exceto desdobramento)
            If !lDesdobr .or. (SE2->E2_TIPO $ MVPROVIS .and. lDesdobr)
                // Se estiver utilizando multiplas naturezas por titulo
                If SE2->E2_MULTNAT == "1"
                    // Apaga as naturezas geradas para o titulo
                    DelMultNat( "SE2", @nHdlPrv, @nTotal, @cArquivo, /*lSoContabiliza*/, /*aCols*/, lUsaFlag, @aFlagCTB )
                Else
                    If lAtuSldNat .And. SE2->E2_FLUXO == 'S'
                        AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, If(SE2->E2_TIPO $ MVPAGANT+"/"+MV_CPNEG,"3","2"), "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, If(SE2->E2_TIPO $ MVABATIM, "+", "-"),,FunName(),"SE2",SE2->(Recno()),nOpc)
                    Endif
                Endif

                dbSelectArea(cAlias)
                nRegAtu := SE2->(Recno())
                //Limpo referencias de apuracao de impostos.
                aRecSE2 := FImpExcTit("SE2",SE2->E2_PREFIXO,SE2->E2_NUM,SE2->E2_PARCELA,SE2->E2_TIPO,SE2->E2_FORNECE,SE2->E2_LOJA,@aTpImp)
                For nX := 1 to Len(aRecSE2)
                    SE2->(MSGoto(aRecSE2[nX]))
                    FaAvalSE2(4,,,,,,,,,,aTpImp[nX])
                Next
                // Exclui os registros de relacionamentos do SFQ
                SE2->(dbGoto(nRegAtu))
                If lPCCBaixa .And. SE2->E2_TIPO $ MVPAGANT //Se for PA (geracao de tx's pela emissao), exclui o SFQ pelo SE5.
                    FImpExcSFQ("SE5",SE2->E2_PREFIXO,SE2->E2_NUM,SE2->E2_PARCELA,SE2->E2_TIPO,SE2->E2_FORNECE,SE2->E2_LOJA)
                Else
                    FImpExcSFQ("SE2",SE2->E2_PREFIXO,SE2->E2_NUM,SE2->E2_PARCELA,SE2->E2_TIPO,SE2->E2_FORNECE,SE2->E2_LOJA)
                Endif

                // Apaga os registros referentes ao rateio do titulo
                If !Empty(SE2->E2_ARQRAT)
                    cChaveCV4 := RTrim(SE2->E2_ARQRAT)
                    RecLock(cAlias ,.F.,.T.)
                    SE2->E2_ARQRAT := "" // Limpa Relacionamento com CV4
                    MsUnlock()
                    FKCOMMIT()
                    CV4->(dbSetOrder(1))
                    If CV4->(MsSeek(cChaveCV4))   // Chave jah contem filial
                        While CV4->(!Eof()) .And.;
                        CV4->CV4_FILIAL+DTOS(CV4->CV4_DTSEQ)+CV4->CV4_SEQUEN == cChaveCV4
                            //Exclui lancamento para o modulo PCO
                            PcoDetLan("000021","01","FINA050",.T.)
                            RecLock("CV4",.F.,.T.)
                            CV4->(dbDelete())
                            MsUnlock()
                            CV4->(DbSkip())
                        End
                    Endif
                Endif

                If  UsaSeqCor()
                    aDiario := {}
                    aDiario := {{"SE2",SE2->(recno()),SE2->E2_DIACTB,"E2_NODIA","E2_DIACTB"}}
                Else
                    aDiario := {}
                EndIf

                nRecnoSE2 := (SE2->(Recno()))

                //Dados da tabela auxiliar com o código do título a pagar
                cChaveTit := xFilial("SE2") + "|" + SE2->E2_PREFIXO + "|" + SE2->E2_NUM     + "|" + SE2->E2_PARCELA + "|" + ;
                                                SE2->E2_TIPO    + "|" + SE2->E2_FORNECE + "|" + SE2->E2_LOJA

                cChaveFK7 := FINGRVFK7("SE2", cChaveTit)


                //realiza a exclusao da tabela complementar
                If __lLocBRA
                    Fa986Excl("SE2")
                EndIf

                If cAlias == "SE2"
                    FINDELFKs(cChaveTit,"SE2")
                Endif

                RecLock(cAlias ,.F.,.T.)
                dbDelete()
                MsUnLock()

                // Com o registro ainda em lock exclui o registro da tabela RC1, pois utiliza a transação ativa.
                If lDelGPE
                    FinDelGPE( SE2->( xFilial("RC1") + E2_FILORIG + E2_PREFIXO + E2_NUM + E2_TIPO + E2_FORNECE) )
                EndIf

                // Atualiza dados do fornecedor
                If lAtuForn
                    cAliasFor	:=	GetNextAlias()
                    cQryFor := "SELECT MAX(E2_EMISSAO) ULTCOM "
                    cQryFor += "FROM "+RetSqlName("SE2")+" WHERE "
                    cQryFor += "E2_FORNECE='"+SA2->A2_COD+"' AND "
                    cQryFor += "E2_LOJA='"+SA2->A2_LOJA+"' AND "
                    cQryFor += "D_E_L_E_T_=' ' "
                    cQryFor += "GROUP BY E2_FORNECE, E2_LOJA "
                    cQryFor := ChangeQuery(cQryFor)
                    dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQryFor),cAliasFor,.F.,.T.)
                    If (cAliasFor)->(Eof())
                        cUltima := ""
                    ELSE
                        cUltima := (cAliasFor)->ULTCOM
                    ENDIF
                    (cAliasFor)->(DBCloseArea())

                    Reclock("SA2")
                    SA2->A2_ULTCOM := STOD(cUltima)
                    MsUnlock()
                Endif
            Endif

            IF nISS != 0 
                // Apaga tambem os registro de impostos-ISS
                dbSelectArea("SE2")
                dbSetOrder(1)
                If dbSeek(xFilial("SE2")+cPrefixo+cNum+cParcIss+MVISS)
                    While !Eof() .And. SE2->(E2_FILIAL + E2_PREFIXO + E2_NUM + E2_PARCELA + E2_TIPO) == xFilial("SE2") + cPrefixo + cNum + cParcIss + "ISS"
                        //Se nao existir E2_TITPAI, valida da forma antiga
                        //Se E2_TITPAI vazio (titulos antigos), valida da forma antiga
                        //Se E2_TITPAI preenchido, pre-valida com a chave do titulo principal
                        If !Empty(SE2->(E2_TITPAI)) .And. SE2->(E2_TITPAI) = cTitPai
                            IF AllTrim(E2_NATUREZ) = AllTrim(&(cMVISS)) .And. SE2->E2_SALDO != 0
                                // Apaga o lancamento do ISS gerado no PCO
                                PCODetLan("000002","09","FINA050",.T.)
                                If lAtuSldNat
                                    AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "-",,FunName(),"SE2",SE2->(Recno()),nOpc)
                                Endif

                                FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                                If lExistFJU
                                    FinGrvEx("P")
                                Endif
                                RecLock("SE2",.F.,.T.)
                                dbDelete()
                                MsUnLock()
                            EndIf
                        Endif
                        dbSkip()
                    Enddo
                Endif
            EndIf

            IF nINSS != 0
                // Apaga tambem os registro de impostos-INSS
                dbSelectArea("SE2")
                dbSetOrder(1)
                dbSeek(xFilial("SE2")+cPrefixo+cNum+cParcInss+IF(cTipo$MVPAGANT,"INA",MVINSS))
                While !Eof( ) .And. E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO == ;
                    cFilial+cPrefixo+cNum+cParcInss+IF(cTipo$MVPAGANT,"INA",MVINSS)
                    IF AllTrim(E2_NATUREZ) = AllTrim(&(cNaturINSS))  .And. SE2->E2_SALDO != 0
                        // Apaga o lancamento do INSS gerado no PCO
                        PCODetLan("000002","07","FINA050",.T.)
                        If lAtuSldNat
                            AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "-",,FunName(),"SE2",SE2->(Recno()),nOpc)
                        Endif

                        FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                        If lExistFJU
                            FinGrvEx("P")
                        Endif
                        RecLock("SE2",.F.,.T.)
                        dbDelete()
                        MsUnLock()
                    ElseIf lF050INS .And. FInsDif(cTitPai)
                        // Apaga o lancamento do INSS gerado no PCO
                        PCODetLan("000002","07","FINA050",.T.)
                        If lAtuSldNat
                            AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "-",,FunName(),"SE2",SE2->(Recno()),nOpc)
                        Endif
                        FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                        If lExistFJU
                            FinGrvEx("P")
                        Endif
                        RecLock("SE2" ,.F.,.T.)
                        dbDelete()
                        MsUnLock()
                    EndIf
                    dbSkip()
                Enddo
            EndIf

            //Gera INSS Patronal
            GetParcINP(@cParcINP,@nValINSSPatr)

            IF nValINSSPatr != 0
                // Apaga tambem os registro de impostos-INSS
                dbSelectArea("SE2")
                dbSetOrder(1)
                dbSeek(xFilial("SE2")+cPrefixo+cNum+cParcINP+"INP")
                While !Eof( ) .And. E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO == ;
                    cFilial+cPrefixo+cNum+cParcINP+"INP"
                    IF AllTrim(SE2->E2_NATUREZ) = AllTrim(&(cNaturINSS))  .And. SE2->E2_SALDO != 0
                        If lAtuSldNat
                            AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "-",,FunName(),"SE2",SE2->(Recno()),nOpc)
                        Endif
                        FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                        If lExistFJU
                            FinGrvEx("P")
                        Endif
                        RecLock("SE2",.F.,.T.)
                        dbDelete()
                        msUnLock()
                    ElseIf lF050INS .And. FInsDif(cTitPai)
                        // Apaga o lancamento do INSS gerado no PCO
                        PCODetLan("000002","07","FINA050",.T.)
                        If lAtuSldNat
                            AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "-",,FunName(),"SE2",SE2->(Recno()),nOpc)
                        Endif
                        FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                        If lExistFJU
                            FinGrvEx("P")
                        Endif
                        RecLock("SE2",.F.,.T.)
                        dbDelete()
                        msUnLock()
                    EndIf
                    dbSkip()
                Enddo
            EndIf

            IF nSEST != 0
                // Apaga tambem os registro de impostos-SEST
                dbSelectArea("SE2")
                dbSetOrder(1)
                dbSeek(cFilial+cPrefixo+cNum+cParcSEST+"SES")
                While !Eof( ) .And. E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO == ;
                    cFilial+cPrefixo+cNum+cParcSEST+"SES"
                    IF AllTrim(E2_NATUREZ) = AllTrim(cSEST)  .And. SE2->E2_SALDO != 0
                        // Apaga o lancamento do SEST/SENAT gerado no PCO
                        PCODetLan("000002","08","FINA050",.T.)
                        If lAtuSldNat
                            AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "-",,FunName(),"SE2",SE2->(Recno()),nOpc)
                        Endif

                        FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                        If lExistFJU
                            FinGrvEx("P")
                        Endif
                        RecLock("SE2",.F.,.T.)
                        dbDelete()
                        msUnLock()
                    EndIf
                    dbSkip()
                Enddo
            EndIf

            If lCIDE .And. nCIDE != 0
				// Deleta os registro de impostos-CIDE
				FDelCIDE( nSavRec, cParcCIDE )

				// Saldos p/ Natureza
			    AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "-",,FunName(),"SE2",SE2->(Recno()))

				// Deleta FKS
				FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
				If lExistFJU
					FinGrvEx("P")
				Endif
            EndIf

            IF nPis != 0 .or. (nPis == 0 .and. nVretPis > 0)
                // Apaga tambem os registro de impostos-PIS
                dbSelectArea("SE2")
                dbSetOrder(1)
                dbSeek(cFilial+cPrefixo+cNum+cParcPis+Iif(cTipoSE2 $ MVPAGANT+"/"+MV_CPNEG .And. !lPCCBaixa,MVTXA,MVTAXA))
                While !Eof( ) .And. E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO == ;
                    cFilial+cPrefixo+cNum+cParcPis+Iif(cTipoSE2 $ MVPAGANT+"/"+MV_CPNEG .And. !lPCCBaixa,MVTXA,MVTAXA)
                    IF AllTrim(E2_NATUREZ) = AllTrim(cNaturPIS)  .And. SE2->E2_SALDO != 0
                        // Apaga o lancamento do PIS gerado no PCO
                        PCODetLan("000002","10","FINA050",.T.)
                        If lAtuSldNat
                            AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "-",,FunName(),"SE2",SE2->(Recno()),nOpc)
                        Endif

                        FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                        If lExistFJU
                            FinGrvEx("P")
                        Endif
                        RecLock("SE2",.F.,.T.)
                        dbDelete()
                        msUnLock()
                    EndIf
                    dbSkip()
                Enddo
            EndIf
            IF nCofins != 0 .or. (nCofins == 0 .and. nVretCof > 0)
                // Apaga tambem os registro de impostos-COFINS
                dbSelectArea("SE2")
                dbSetOrder(1)
                dbSeek(cFilial+cPrefixo+cNum+cParcCof+Iif(cTipoSE2 $ MVPAGANT+"/"+MV_CPNEG .And. !lPCCBaixa,MVTXA,MVTAXA))
                While !Eof( ) .And. E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO == ;
                    cFilial+cPRefixo+cNum+cParcCof+Iif(cTipoSE2 $ MVPAGANT+"/"+MV_CPNEG .And. !lPCCBaixa,MVTXA,MVTAXA)
                    IF AllTrim(E2_NATUREZ) = AllTrim(cNaturCOF)  .And. SE2->E2_SALDO != 0
                        // Apaga o lancamento do COFINS gerado no PCO
                        PCODetLan("000002","11","FINA050",.T.)
                        If lAtuSldNat
                            AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "-",,FunName(),"SE2",SE2->(Recno()),nOpc)
                        Endif

                        FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                        If lExistFJU
                            FinGrvEx("P")
                        Endif
                        RecLock("SE2",.F.,.T.)
                        dbDelete()
                        msUnLock()
                    EndIf
                    dbSkip()
                Enddo
            EndIf
            IF nCsll != 0  .or. (nCsll == 0 .and. nVretCsl > 0)
                // Apaga tambem os registro de impostos-CSLL
                dbSelectArea("SE2")
                dbSetOrder(1)
                dbSeek(cFilial+cPrefixo+cNum+cParcCsll+Iif(cTipoSE2 $ MVPAGANT+"/"+MV_CPNEG .And. !lPCCBaixa,MVTXA,MVTAXA))
                While !Eof( ) .And. E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO == ;
                    cFilial+cPrefixo+cNum+cParcCsll+Iif(cTipoSE2 $ MVPAGANT+"/"+MV_CPNEG .And. !lPCCBaixa,MVTXA,MVTAXA)
                    IF AllTrim(E2_NATUREZ) = AllTrim(cNaturCSLL)  .And. SE2->E2_SALDO != 0
                        // Apaga o lancamento do CSLL gerado no PCO
                        PCODetLan("000002","12","FINA050",.T.)
                        If lAtuSldNat
                            AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "-",,FunName(),"SE2",SE2->(Recno()),nOpc)
                        Endif

                        FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                        If lExistFJU
                            FinGrvEx("P")
                        Endif
                        RecLock("SE2",.F.,.T.)
                        dbDelete()
                        msUnLock()
                    EndIf
                    dbSkip()
                Enddo
            EndIf
            IF cPaisLoc $ "DOM|COS"
                // Apaga os registro de impostos-IRF          |
                dbSelectArea("SE2")
                SE2->(dbSeek(cFilial+cPrefixo+cNum+cParcela))
                While !SE2->(Eof( )) .And. E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA == ;
                    cFilial+cPRefixo+cNum+cParcela
                    IF AllTrim(E2_TIPO) $ "IR-|IRF|ISR|IS-|IT |IT-"  .And. SE2->E2_SALDO != 0
                        If lAtuSldNat
                            AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "-")
                        Endif

                        FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                        RecLock("SE2",.F.,.T.)
                        SE2->(dbDelete( ))
                        msUnLock()
                    EndIf
                    SE2->(dbSkip())
                Enddo
            EndIf

            // Apaga tambem os registros agregados-SE2
            If !( cTipoSE2 $ MVPAGANT+"/"+MV_CPNEG+"/"+MVABATIM)
                dbSelectArea("SE2")
                dbSetOrder(1)
                dbSeek(cFilial+cPrefixo+cNum+cParcela)
                While !EOF() .And. E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA == ;
                    cFilial+cPrefixo+cNum+cParcela
                    IF SE2->E2_TIPO $ MVABATIM .and. E2_FORNECE == cFornece
                        If lAtuSldNat .And. SE2->E2_FLUXO == 'S'
                            AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "+",,FunName(),"SE2",SE2->(Recno()),nOpc)
                        Endif
                        FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                        RecLock("SE2",.F.,.T.)
                        If lPadrao .and. !lDesdobr .And. SubStr(SE2->E2_LA,1,1) == "S"
                            // Prepara Lancamento Contabil
                            If lUsaFlag  // Armazena em aFlagCTB para atualizar no modulo Contabil
                                aAdd( aFlagCTB, {"E2_LA", "S", "SE2", SE2->( Recno() ), 0, 0, 0} )
                            Endif
                            nTotal += DetProva( nHdlPrv,;
                            cPadrao,;
                            "FINA050" /*cPrograma*/,;
                            cLote,;
                            /*nLinha*/,;
                            /*lExecuta*/,;
                            /*cCriterio*/,;
                            /*lRateio*/,;
                            /*cChaveBusca*/,;
                            /*aCT5*/,;
                            /*lPosiciona*/,;
                            @aFlagCTB,;
                            /*aTabRecOri*/,;
                            /*aDadosProva*/ )
                        Endif
                        dbDelete()
                        msUnLock()
                        If lAtuForn
                            Reclock("SA2")
                            SA2->A2_SALDUP  += Round(NoRound(xMoeda(SE2->E2_SALDO,SE2->E2_MOEDA,1,SE2->E2_EMISSAO,3),3),2)
                            SA2->A2_SALDUPM += Round(NoRound(xMoeda(SE2->E2_SALDO,SE2->E2_MOEDA,nMoeda,SE2->E2_EMISSAO,3),3),2)
                            MsUnlock()
                        EndIf
                        dbSelectArea( "SE2" )
                    EndIf
                    dbSkip()
                Enddo
            Endif

            // Não devera excluir titulos TX amarrados manualmente com PR.
            If (!cNatureza $ &(cNaturIRF)) .AND. (!cTipoSE2 $ MVPROVIS)
                // Apaga tambem os registro de impostos
                dbSelectArea("SE2")
                nOrd		:= IndexOrd()
                dbSetOrder(17)  // E2_FILIAL + E2_TITPAI
                dbSeek(cFilial+cTitPai)
                While !EOF() .And. Alltrim(E2_FILIAL+E2_TITPAI) == Alltrim(cFilial+cTitPai)
                    
                    IF E2_NATUREZ = &(cNaturIRF) .And. SE2->E2_SALDO != 0 .and. Alltrim(SE2->E2_TIPO) == Alltrim(IIF(cTipo $ MVPAGANT+"/"+MV_CPNEG .And. ! lIRPFBaixa,MVTXA,MVTAXA))

                        If !(FwIsInCallStack("FINA631") .AND. SED->ED_CALCIRF == 'N') // Não deletar titulo de IR TX de solicitação de transferência quando estornar o NF

                            // Apaga o lancamento do IRRF gerado no PCO
                            PCODetLan( "000002", "06", "FINA050", .T. )
                            If lAtuSldNat
                                AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "-",,FunName(),"SE2",SE2->(Recno()),nOpc)
                            Endif

                            FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                            If lExistFJU
                                FinGrvEx("P")
                            Endif
                            RecLock("SE2",.F.,.T.)
                            dbDelete()
                            msUnLock()

                        EndIf
                    EndIF
                    dbSkip()
                EndDo
                dbSetOrder(nOrd)
            EndIf
            // Finaliza a gravacao dos lancamentos do SIGAPCO
            PcoFinLan("000002")
            If nTotal > 0
                If lF050Auto
                    lSetAuto := _SetAutoMode(.F.)
                    lSetHelp := HelpInDark(.F.)
                    If Type('lMSHelpAuto') == 'L'
                        lMSHelpAuto := !lMSHelpAuto
                    EndIf
                EndIf

                // Envia para Lan‡amento Contabil -
                cA100Incl( cArquivo,;
                nHdlPrv,;
                3 /*nOpcx*/,;
                cLote,;
                ( mv_par01 == 1 ) /*lDigita*/,;
                ( mv_par07 == 1 ) /*lAglut*/,;
                /*cOnLine*/,;
                /*dData*/,;
                /*dReproc*/,;
                @aFlagCTB,;
                /*aDadosProva*/,;
                aDiario )
                aFlagCTB := {}  // Limpa o coteudo apos a efetivacao do lancamento

                If lFA050RAT
                    ExecBlock ("FA050RAT",.f.,.f.)
                EndIf

                If lF050Auto
                    HelpInDark(lSetHelp)
                    _SetAutoMode(lSetAuto)
                    If Type('lMSHelpAuto') == 'L'
                        lMSHelpAuto := !lMSHelpAuto
                    EndIf
                EndIf
            Endif

            //Executa rotina para estorno de titulo provisorio
            If lEstProv .and. nRecnoSE2 > 0
                F050RetPR(nRecnoSE2)

                If !lF050Auto .and. FindFunction("F181EstMov")                    
                    If !F181EstMov(nSavRec)
                        DisarmTransaction()
                        Break
                    Endif
                Endif    
            EndIF

            If SE2->(Recno()) <> nSavRec
                If (E2_PRETPIS	= '2' .Or. E2_PRETCOF	= '2' .Or. E2_PRETCSL	= '2') .and. !lFina250
                    If	F050VerAlt()
                        lTitRetD := .T.
                    Else
                        lTitRetD := .F.
                    EndIf
                Else
                    lTitRetD := .F.
                EndIf
            EndIf

            If !lPCCBaixa .And. lTitRetD
                SE2->(dbGoto(nSavRec))
                F050DelRtd()
            EndIf
            // Colocar o Status na Solicitação se for PA gerardo na Solicitação de fundos
            If nRecnoFJA <> 0
                aArea   := GetArea()
                dbSelectArea("FJA")
                FJA->(DbGoto(nRecnoFJA))
                RecLock("FJA" ,.F.)
                FJA->FJA_ESTADO := "2"
                FJA->FJA_PREFIX := ""
                FJA->FJA_NUMTIT := ""
                FJA->FJA_PARCEL := ""
                FJA->FJA_TIPO 	:= ""
                FJA->(MsUnlock())
                RestArea(aArea)
            Endif

            //-------------------------------------------------------------------------------------
            // Integração Gesplan - Update somente para atualização do timestamp do registro pai
            SE2->(dbGoto(nSavRec))
            If __lGesplan .And. ( SE2->E2_TIPO $ MVABATIM )
                
                If !Empty(SE2->E2_TITPAI) .And. SE2->(MsSeek(xFilial("SE2") + SE2->E2_TITPAI))
                    FUpdStamp('SE2',SE2->(Recno()))
                EndIF	
                SE2->(MsGoto(nSavRec)) //reposiciono no AB-
            EndIF            


        // Final do bloco protegido via TTS.
        End Transaction

        //Finaliza o processo do lancamento no Pco quando E2_RATEIO == "S"
        If lRateioPCO .and. lRet
            PcoFinLan("000021")
        EndIf
    Else
        MsUnlock()
        dbSelectArea(cAlias)
        dbGoto(nProxReg)
        dbSetOrder(nIndex)
        Return .F.
    Endif

    // Verifica o arquivo de rateio, e deleta o conteúdo do arquivo temporario
    // para que no proximo rateio seja reutilizado a mesma tabela no banco
    If Select("TMP") > 0 .And. !Empty(__cFIN1Name)
        nTcSql := TcSQLExec("DELETE FROM "+__cFIN1Name)
        FChkTCExec(nTcSql, 1)
    EndIf

    If __lLocBRA
        F986LimpaVar() //Limpa as variaveis estaticas - Complemento de Titulo
        f050LRatIR(.T.)
    EndIf

    If IntePMS() .And. SE2->E2_PROJPMS == "1"
        SetKey(VK_F10, Nil)
    EndIf

    FWFreeArray(aBut050)
    FWFreeArray(aDiario)
    FWFreeArray(aTitImp)
    FWFreeArray(aArea)
    FWFreeArray(aTpImp)
    FWFreeArray(aFlagCTB)
    FWFreeArray(aAreaSE2)
    FWFreeArray(aAreaSA2)
    FWFreeArray(aChave)
    FWFreeArray(aPenCont)
    FWFreeArray(aEaiRet)
    FWFreeArray(aAreaAnt)
    FWFreeArray(aExcSE3)

    dbSelectArea(cAlias)
    dbGoto(nProxReg)
    dbSetOrder(nIndex)

Return .T.

//-------------------------------------------------------------------
/*/{Protheus.doc} FA050Tipo

Checa o Tipo do titulo informado

@author Wagner Xavier
@since  30/04/92
@version 12
@param lTudook     Lógico       Indica se chamado pelo TudoOk
/*/
//-------------------------------------------------------------------
Function FA050Tipo(lTudook As Logical) As Logical 

    LOCAL lRetorna  As Logical
    Local lAchou    As Logical
    LOCAL cChaveSe2 As Character
    Local cChaveAba As Character
    LOCAL cSEST     As Character
    Local cBuscaSe2 As Character

    lRetorna  := .T.
    cChaveSe2 := ""
    cChaveAba := ""
    cSEST     := GetMv("MV_SEST",,"")
    cBuscaSe2 := ""
    lAchou    := .F. 

    Default lTudook := .F.      // define se a chamada vem do TudoOk

    If cPaisLoc $ "BRA|MEX"
        If Type("aRecnoAdt") != "U" .and. (FunName() = "MATA121" .or. FunName() = "MATA103")
            If !M->E2_TIPO $ MVPAGANT
                Aviso(STR0026,STR0168,{ "Ok" }) // "ATENCAO"#"Por tratar-se de título para processo de adiantamento, é obrigatório que o tipo do título seja 'PA', ou a correspondente a adiantamento."
                Return(.F.)
            Endif
        Endif
    Endif

    dbSelectArea("SE2")
    nRegistro:=Recno()
    dbSetOrder(1)
    cTipoParaAbater := SE2->E2_TIPO

    cChaveSe2 := "'" + cFilial + "' + m->e2_prefixo + m->e2_num + " +;
    "m->e2_parcela + m->e2_tipo + m->e2_fornece + m->e2_loja"

    If Len(FWGetSX5("05",M->E2_TIPO)) == 0
        Help(" ",1,"E2_TIPO")
        lRetorna := .F.
    Elseif !NewTipCart(M->E2_TIPO,"2")
        Help(" ",1,"TIPOCART")
        lRetorna := .F.
    Else
        dbSelectArea("SE2")
        // Se for abatimento, herda os dados do titulo
        If (cPaisLoc <> "PER" .And. M->E2_TIPO $ MVABATIM) .Or. (cPaisLoc == "PER" .AND. M->E2_TIPO $ StrTran( MVABATIM , "IR-|" , "" ))
            If ! Empty(m->e2_num) // Caso o numero seja digitado busco baseado na chave
                // em memoria, caso contrario utilizo o registro posicionado
                // no browse
                If Empty(m->e2_fornece) .Or. Empty(m->e2_loja)
                    DbSetOrder(1) // Altero a ordem para busca sem cliente/loja
                    // e Removo da chave o tipo, Fornecedor e Loja
                    // Caso o fornecedor/loja nao tenham sido digitados
                    cChaveAba := StrTran(cChaveSe2, " + m->e2_tipo + m->e2_fornece + m->e2_loja", "")
                    cBuscaSe2 := StrTran(cChaveAba, "m->", "")
                Else
                    DbSetOrder(6) // E2_FILIAL+E2_FORNECE+E2_LOJA+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO                                                                                               
                    cChaveAba :=  "'" + cFilial + "'+ m->e2_fornece + m->e2_loja + m->e2_prefixo + m->e2_num + m->e2_parcela"
                    cBuscaSe2 := StrTran(cChaveAba, "m->", "")
                Endif

                If !(dbSeek(&cChaveAba))
                    Help(" ",,"FA050TIT",,STR0352,1,0,,,,,, {STR0353})//"Não foi encontrado um título correspondente para vincular este abatimento."#"Insira as informações de um título existente."
                    lRetorna:=.F.
                Endif
            Endif

            // Verifico se não tem titulo com a mesma chave 
            If !(cPaisLoc $ "DOM|COS")
                While !EOF() .And. &cChaveAba == &cBuscaSe2  
                    If !SE2->E2_TIPO $ MVABATIM+"/"+ MVPAGANT+"/"+MVPROVIS+"/"+MV_CPNEG
                        lAchou := .T.
                        Exit
                    EndIf
                    DbSkip()
                Enddo
                If !lAchou // se nao achou titulo retorna para o titulo original
                    (dbSeek(&cChaveAba))
                EndIf
             EndIf
            // Caso seja titulo de adiantamento, nao posso gerar tit.abatimento
            If lRetorna .And. SE2->E2_TIPO $ MVPAGANT+"/"+MVPROVIS+"/"+MV_CPNEG  .And. !(cPaisLoc $ "DOM|COS")
                Help(" ",1,"FA050TITAB",,STR0354,1,0,,,,,, {STR0355})//"Não é possível vincular um abatimento à um Pagamento Antecipado, Título Provisório ou Nota de Débito ao Fornecedor."#"Informe os dados de um título válido para vincular este abatimento."
                dbGoTo(nRegistro)
                lRetorna:=.F.
            Endif

            If M->E2_TIPO $ MVABATIM .And. lRetorna
                If !lF050auto
                    If !__SelPai
                        iF !F050VlAbt()
                            lRetorna := .F.
                        Endif
                    EndIf
                Else
                    If !lTudook // Não chamar pelo tudook quando execauto para não sobrepor valores passados no execauto
                        FA050Herda()
                    EndIf
                    cTitPaiAB := SE2->(E2_PREFIXO + E2_NUM + E2_PARCELA + E2_TIPO + E2_FORNECE + E2_LOJA)
                    cTipoParaAbater := SE2->E2_TIPO

                    If SE2->( dbSeek(&cChaveSe2) )
                        Help(" ",1,"FA050NUM")
                        m->e2_num := CRIAVAR("E2_NUM")
                        lRetorna := .F.
                    Endif
                EndiF
            Endif
        EndIf

        If SE2->( dbSeek(&cChaveSe2) )
            Help(" ",1,"FA050NUM")
            dbGoTo(nRegistro)
            lRetorna:=.F.
        Else
            dbGoTo(nRegistro)
        Endif

        If lRetorna .And. m->e2_naturez$&(GetMv("MV_IRF")) .And. !m->e2_tipo $ MVTAXA
            Help(" ",1,"E2_TIPO")
            lRetorna := .F.
        EndIf

        If lRetorna .And. m->e2_naturez$&(GetMv("MV_INSS")) .And. !m->e2_tipo $ MVINSS
            Help(" ",1,"E2_TIPO")
            lRetorna := .F.
        EndIf

        If lRetorna .And. m->e2_naturez$AllTrim(cSEST) .And. !m->e2_tipo $ "SES"
            Help(" ",1,"E2_TIPO")
            lRetorna := .F.
        EndIf

        If lRetorna .And. m->e2_tipo $ MVRECANT+"/"+MV_CRNEG
            Help(" ",1,"E2_TIPO")
            lRetorna := .F.
        EndIf
    EndIf

    If lRetorna .And. M->E2_TIPO $ MVPAGANT .AND. ! lF050Auto
        Fa050DigPa(,@M->E2_MOEDA,Iif(Type("lSubst")=="L",lSubst,.F.))
    Endif

Return lRetorna

//-------------------------------------------------------------------
/*/{Protheus.doc}FA050Venc

Verifica a data de vencimento informada

@author Wagner Xavier
@since  29/05/92
@version 12
/*/
//-------------------------------------------------------------------
Function FA050Venc(nTpVenc As Numeric) As Logical

Local lRetorna  As Logical 
Local lPCCBaixa As Logical 

DEFAULT nTpVenc := 1 //1= Validando E2_VENCTO, 2 = Validando E2_VENCREA

lRetorna  := .T.
lPCCBaixa := SuperGetMv("MV_BX10925",.T.,"2") == "1"

If Type("lAltera")=="U"
    lAltera := .T.
    dVencReaAnt := CtoD('')
EndIf

If __lLocBRA .And. SA2->A2_RECPIS=="1" .AND. SA2->A2_RECCOFI=="1" .AND. SA2->A2_RECCSLL=="1"
    aFill(aDadosImp,0)
Endif

If lRetorna
    //Validando data de vencto
    If M->E2_VENCTO < M->E2_EMISSAO
        Help(" ",1,"FANODATA")
        lRetorna := .F.

        //Validando data de vencimento Real
    ElseIf nTpVenc == 2 .and. M->E2_VENCREA < M->E2_VENCTO
        lRetorna := .F.
        MsgAlert(STR0230)

        //Caso o titulo tenha sido contabilizado, nao podera ser alterado em nada
        //que influencie no calculo dos impostos
    ElseIf __lLocBRA .and. lAltera .and. !lPccBaixa

        //Apenas para E2_VENCTO
        //Atualizacao do E2_VENCREA
        If nTpVenc == 1
            M->E2_VENCREA := DataValida(M->E2_VENCTO,.T.)
        Endif

        //Verifico se houve mudança de mes para o PCC
        If	(Month(M->E2_VENCREA) <> Month(dVencReaAnt))

            nRecAtu := SE2->(RECNO())
            //Busco a informacao de qual o titulo retentor do PCC do titulo em alteracao
            SFQ->(DbSetOrder(2)) //-- FQ_FILIAL+FQ_ENTDES+FQ_PREFDES+FQ_NUMDES+FQ_PARCDES+FQ_TIPODES+FQ_CFDES+FQ_LOJADES
            If SFQ->(DbSeek(xFilial("SFQ")+"SE2"+SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)))

                //Posiciono no cadastro de C.Pagar para verificar se o titulo retentor
                //foi contabilizado ou veio de outro modulo
                SE2->(DbSetOrder(1)) //-- E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA
                If SE2->(DbSeek(xFilial("SE2")+SFQ->(FQ_PREFORI+FQ_NUMORI+FQ_PARCORI+FQ_TIPOORI+FQ_CFORI+FQ_LOJAORI)))

                    //Titulos contabilizados
                    //os titulos vindos de outros modulos sempre tem E2_LA = 'S' ja que a contabilizacao ocorre na origem
                    If SE2->E2_LA == "S"
                        /*
                        Conforme chamado TRGZT8, passou-se a permitir a alteracao da data de vencimento, porem, os impostos nao sao recalculados. */
                        lRetorna := .T.
                        Help(,,"ALTERVENCTO",,STR0245,1,0)		//"O título retentor de impostos está contabilizado ou foi gerado em outro módulo. Será permitida a alteração da data de vencimento, porém, os impostos não serão recalculados."
                    Endif
                Endif
            Else
                //Verifico se o titulo eh retentor do PCC de outros titulos
                SFQ->(DbSetOrder(1)) //-- FQ_FILIAL+FQ_ENTORI+FQ_PREFORI+FQ_NUMORI+FQ_PARCORI+FQ_TIPOORI+FQ_CFORI+FQ_LOJAORI
                If SFQ->(DbSeek(xFilial("SFQ")+"SE2"+SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)))
                    //Titulos contabilizados
                    //os titulos vindos de outros modulos sempre tem E2_LA = 'S' ja que a contabilizacao ocorre na origem
                    If SE2->E2_LA == "S"
                        //Help(" ",1,"F050ALPCC")
                        /*
                        Conforme chamado TRGZT8, passou-se a permitir a alteracao da data de vencimento, porem, os impostos nao sao recalculados. */
                        lRetorna := .T.
                        Help(,,"ALTERVENCTO",,STR0245,1,0)		//"O título retentor de impostos está contabilizado ou foi gerado em outro módulo. Será permitida a alteração da data de vencimento, porém, os impostos não serão recalculados."
                    Endif
                Endif
            Endif
            dbSelectArea("SE2")
            SE2->(DbGoto(nRecAtu))
            // Quando alterou o mes e PCC na emissão *
            // Utilizado para calcular o pcc quando  *
            // data alterada na inclusão.            *
            If (Month(M->E2_VENCREA) <> Month(dVencReaAnt)) .and. nTpVenc==2 .and. M->E2_LA != "S" .and. M->E2_VALOR == M->E2_SALDO
                If ! ( Alltrim(SE2->E2_ORIGEM) == "FINA290" .And. Alltrim(SE2->E2_FATURA)  == "NOTFAT" )
                    FA050Nat2()
                EndIf 
                dVencReaAnt := M->E2_VENCREA
            EndIf

        Endif

    ElseIf __lLocBRA .and. INCLUI .and. !lPccBaixa
        // Quando alterou o mes e PCC na emissão *
        // Utilizado para calcular o pcc quando  *
        // data alterada na inclusão.            *
        If (Month(M->E2_VENCREA) # Month(dVencReaAnt)) .and. M->E2_LA != "S"
            FA050Nat2()
            dVencReaAnt := M->E2_VENCREA
        EndIf

    Endif

    If lRetorna

        //Vencimento original do titulo
        If Empty(SE2->E2_VENCORI)
            M->E2_VENCORI := M->E2_VENCTO
        EndIf

        //Apenas para E2_VENCTO
        //Atualizacao do E2_VENCREA
        If nTpVenc == 1
            M->E2_VENCREA := DataValida(M->E2_VENCTO,.T.)
        Endif

        //Data de Agendamento do titulo
        M->E2_DATAAGE := M->E2_VENCREA

        lRefresh := .T.

    EndIf
EndIf

Return lRetorna

//-------------------------------------------------------------------
/*/{Protheus.doc} Fa050Visua

Programa para visualizacao de contas a pagar

@cAlias = Alias do arquivo
@nReg = Numero do registro
@nOpc = Numero da opcao selecionada
@author Wagner Xavier
@since  16/04/92
@version 12
/*/
//-------------------------------------------------------------------
Function Fa050Visua( cAlias As Character, nReg As Numeric, nOpc As Numeric )

    Local aBut050  As Array
    LOCAL nOpcA    As Numeric 
    Local lF050VIS As Logical

    PRIVATE aRatAFR		As Array
    Private aSE2FI2		As Array 
    Private aCposAlter  As Array
    PRIVATE bPMSDlgFI	As Block
    PRIVATE _Opc 		As Numeric 

    Default __lIntPFS  := SuperGetMv("MV_JURXFIN",.T.,.F.) //Integração do Financeiro com o Juridico(Habilitado = .T.)
    Default __lTemMR   := (FindFunction("FTemMotor") .and. FTemMotor())

    nOpcA    := 0
    aBut050  := {}
    lF050VIS := Existblock("F050VIS")

    aRatAFR		:= {}
    bPMSDlgFI	:= {||PmsDlgFI(2,M->E2_PREFIXO,M->E2_NUM,M->E2_PARCELA,M->E2_TIPO,M->E2_FORNECE,M->E2_LOJA)}
    _Opc 		:= nOpc
    aSE2FI2		:=	{} // Utilizada para gravacao das justificativas
    aCposAlter  :=  {}    

    dbSelectArea("SA2")
    dbSeek(cFilial+SE2->E2_FORNECE+SE2->E2_LOJA)

    //Botoes adicionais na EnchoiceBar
    aBut050 := fa050BAR('SE2->E2_PROJPMS == "1"')

    ///Projeto
    //inclusao do botao Posicao
    AADD(aBut050, {"HISTORIC", {|| Fc050Con() }, STR0204}) //"Posicao"

    //inclusao do botao Rastreamento
    AADD(aBut050, {"HISTORIC", {|| Fin250Pag(2) }, STR0205}) //"Rastreamento"

    If __lIntPFS .And. FindFunction("JURA246") .And. !(SE2->E2_TIPO $ MVTAXA+"|"+MVINSS+"|"+MVISS+"|"+MVTXA+"|SES|INA|IRF|PIS|COF|CSL")
        Aadd(aBut050,{"", {|| JURA246(1) }, STR0296}) //"Detalhe / Desdobramentos" (Módulo SIGAPFS)
    EndIF

    //Motor de retenções
    If __lTemMR
        AADD(aBut050, {"HISTORIC", {|| FINCRET('SE2') }, STR0300}) //'Consulta de Retenções'
    EndIF

    // integração com o PMS
    If IntePMS() .And. SE2->E2_PROJPMS == "1"
        SetKey(VK_F10, {|| Eval(bPMSDlgFI)})
    EndIf
    dbSelectArea(cAlias)
    RegToMemory("SE2",.T.,,.F.,FunName())
    nOpca := AxVisual(cAlias,nReg,nOpc,,4,SA2->A2_NOME,"FA050MCPOS",aBut050)
    If lF050VIS		// ponto na saida da visualizacao
        Execblock("F050VIS",.f.,.f.)
    Endif

    If IntePMS() .And. SE2->E2_PROJPMS == "1"
        SetKey(VK_F10, Nil)
    EndIf
    If __lLocBRA
        F986LimpaVar() //Limpa as variaveis estaticas - Complemento de Titulo
    EndIf

Return

//-------------------------------------------------------------------
/*/{Protheus.doc} Fa050Subst

Rotina para substituicao de titulos provisorios.

@cAlias = Alias do arquivo
@nReg = Numero do registro
@nOpc = Numero da opcao selecionada
@author Mauricio Pequim Jr
@since  06/08/2019
@version 12
/*/
//-------------------------------------------------------------------
Function Fa050Subst(cAlias AS Character, nReg AS Numeric, nOpc AS Numeric)

    LOCAL aFlagCTB   as Array
    LOCAL aMoedas    as Array
    LOCAL aOutMoed   as Array //"1=Nao Considera"###"2=Converte"
    LOCAL cArquivo   as Character
    LOCAL cMoeda     as Character
    LOCAL cOutMoeda  as Character
    LOCAL cPadrao    as Character
    LOCAL cSimb      as Character
    LOCAL lAtuSldNat as Logical
    LOCAL lPadrao    as Logical
    LOCAL lUsaFlag   as Logical
    LOCAL nHdlPrv    as Numeric
    LOCAL nI         as Numeric
    LOCAL nPosFor    as Numeric
    LOCAL nPosLoj    as Numeric
    LOCAL nPosNum    as Numeric
    LOCAL nPosPar    as Numeric
    LOCAL nPosPre    as Numeric
    LOCAL nPosTip    as Numeric
    LOCAL nRecSE2    as Numeric
    LOCAL nTotal     as Numeric
    LOCAL nValorSe2  as Numeric
    LOCAL oCbx       as Object
    LOCAL oCbx2      as Object
    LOCAL oDlg       as Object
    LOCAL oQtdTit    as Object
    LOCAL oValor     as Object

    //Ponto de entrada para deletar provisorios ao inves de baixa-los
    LOCAL lF50DelPr     AS Logical
    LOCAL lDelProvis    AS Logical
    //Substituicao automatica
    Local a1stRow   as Array
    Local a2ndRow   as Array
    Local aAreaSE2  as Array
    Local aAreaSubs as Array
    Local aBut050S  as Array
    LOCAL aChaveLbn as Array
    Local aGravaAFR as Array
    Local aNtit     as Array
    LOCAL aSelFil   as Array
    LOCAL aTmpFil   as Array
    LOCAL cCfDest   as Character // Armazena cliente/fornecedor do titulo NF
    LOCAL cCfOri    as Character // Armazena cliente/fornecedor do titulo PR
    LOCAL cFIISeq   as Character // Armazena Sequencial gerado na baixa (SE5)
    LOCAL cFilDest  as Character // Armazena filial de destino do titulo NF
    LOCAL cLojaDest as Character // Armazena loja do titulo NF
    LOCAL cLojaOri  as Character // Armazena loja do titulo PR
    LOCAL cNumDest  as Character // Armazena numero do titulo NF
    LOCAL cNumOri   as Character // Armazena numero do titulo PR
    LOCAL cParcDest as Character // Armazena parcela do titulo NF
    LOCAL cParcOri  as Character // Armazena parcela do titulo PR
    LOCAL cPrefDest as Character // Armazena prefixo do titulo NF
    LOCAL cPrefOri  as Character // Armazena prefixo do titulo PR
    LOCAL cTipoDest as Character // Armazena tipo do titulo NF
    LOCAL cTipoOri  as Character // Armazena tipo do titulo PR
    LOCAL dDtEmiss  as Date // Variavel para armanzenar a data de emissao do titulo
    Local lRet      as Logical
    Local nIndice   as Numeric //Guardo o indice de entrada para a rotina de pesquisa
    Local nMaxTam   as Numeric
    LOCAL nOpca     as Numeric
    Local nRegSel   as Numeric
    LOCAL nX        as Numeric
    Local oButton   as Object
    Local oSize     as Object
    
    PRIVATE aTitulo2CC as Array //Russia
    PRIVATE cCodFor    as Character
    PRIVATE cLojaFor   as Character
    PRIVATE cNomeFor   as Character
    PRIVATE lSubs      as Logical
    PRIVATE nMoedSubs  as Numeric
    PRIVATE nQtdTit    as Numeric
    PRIVATE nValorS    as Numeric
    PRIVATE oMark      as Object

    Default __lMetric  := FwLibVersion() >= "20210517"
    Default __lF50PROV := ExistBlock("F050PROV")
    Default __lPmsInt  := IsIntegTop(,.T.)

    lPadrao     := .F.
    cPadrao     := "533"
    cArquivo    := ""
    nHdlPrv     := 0
    nTotal      := 0
    nValorSe2	:= 0
    oValor	    := NIL
    oQtdTit	    := NIL
    oDlg        := NIL
    nRecSE2	    := SE2->(RECNO())
    aMoedas	    := {}
    aOutMoed	:= {STR0107,STR0108}	//"1=Nao Considera"###"2=Converte"
    cOutMoeda	:= "1"
    oCbx        := NIL
    oCbx2       := NIL
    cMoeda	    := "1"
    cSimb		:= ""
    aFlagCTB	:= {}
    lUsaFlag	:= SuperGetMV( "MV_CTBFLAG" , .T. /*lHelp*/, .F. /*cPadrao*/)
    lAtuSldNat  := .T.
    nI		    :=  0
    nPosPre	    :=  0
    nPosNum	    :=  0
    nPosPar	    :=  0
    nPosTip	    :=  0
    nPosFor	    :=  0
    nPosLoj	    :=  0
    lF50DelPr   := ExistBlock("F50DELPR")
    lDelProvis  := If(lF50DelPr, ExecBlock("F50DELPR",.F.,.F.), .F.)
    cFIISeq	    := ""   // Armazena Sequencial gerado na baixa (SE5)
    cPrefOri    := ""   // Armazena prefixo do titulo PR
    cNumOri     := ""   // Armazena numero do titulo PR
    cParcOri    := ""   // Armazena parcela do titulo PR
    cTipoOri    := ""   // Armazena tipo do titulo PR
    cCfOri      := ""   // Armazena cliente/fornecedor do titulo PR
    cLojaOri    := ""   // Armazena loja do titulo PR
    cPrefDest   := ""   // Armazena prefixo do titulo NF
    cNumDest    := ""   // Armazena numero do titulo NF
    cParcDest   := ""   // Armazena parcela do titulo NF
    cTipoDest   := ""   // Armazena tipo do titulo NF
    cCfDest     := ""   // Armazena cliente/fornecedor do titulo NF
    cLojaDest   := ""   // Armazena loja do titulo NF
    cFilDest	:= ""   // Armazena filial de destino do titulo NF
    dDtEmiss    := dDatabase  // Variavel para armanzenar a data de emissao do titulo
    aNtit		:= {}
    aGravaAFR   := {}
    nMaxTam	    := 0
    nRegSel	    := 0
    aAreaSE2    := (cAlias)->(GetArea())
    oSize       := NIL
    a1stRow	    := {}
    a2ndRow	    := {}
    oButton     := NIL
    lRet        := .T.
    aBut050S    := {}
    nIndice     := SE2->(IndexOrd())   //Guardo o indice de entrada para a rotina de pesquisa
    aChaveLbn   := {}
    nOpca       := 0
    nX          := 0
    aSelFil	    := {}
    aTmpFil     := {}

    oMark		:= NIL
    nValorS		:= 0
    nQtdTit 	:= 0
    cCodFor		:= CriaVar("A2_COD",.F.)
    cLojaFor	:= CriaVar("A2_LOJA")
    cNomeFor	:= CriaVar("A2_NREDUZ",.F.)
    lSubs		:= .F.
    nMoedSubs	:= 1
    aTitulo2CC  := {} //Russia

    cMarca := GetMark( )
    lPadrao := VerPadrao(cPadrao)
    lDelProvis := If(ValType(lDelProvis) != "L",.F.,lDelProvis)
    VALOR := 0
    cAliasSE2 := "__SUBS"

    // Verifica se data do movimento não ‚ menor que data limite de
    // movimentacao no financeiro
    If !DtMovFin(,,"1")
        lRet := .F.
    Endif

    // A ocorrencia 23 (ACS), verifica se o usuario poder  ou não efetuar substituição de titulos provisórios.
    IF lRet .and. !ChkPsw(23)
        If lF050Auto 
            AutoGRLog(STR0242)
        EndIf
        lRet := .F.
    EndIf

    If lRet
        __cFunBkp := FunName()
        __cFunMet := Iif(AllTrim(__cFunBkp)=='RPC',"RPCFINA050",__cFunBkp)

        If __lMetric
            SetFunName(__cFunMet)
            // Metrica de controle de acessos 
            FwCustomMetrics():setSumMetric(Alltrim(ProcName()), "financeiro-protheus_qtd-por-acesso_total", 1)
            SetFunName(__cFunBkp)
        Endif

        If !lF050Auto
            //Gestao - Selecao de filiais
            aSelFil	:= {}
            aSelFil := AdmGetFil(.F.,.T.,"SE2")
            If Len( aSelFil ) <= 0
                lRet := .F.
            EndIf

            If lRet
                // Inicializa array com as moedas existentes.
                aMoedas := FDescMoed()

                aBut050S := {{"PESQUISA",{||Fa050Pesq(oMark,cAliasSE2,nIndice)}, STR0322, STR0001}} //"Pesquisar..(CTRL-P)"###"Pesquisar"
                bSet16 := SetKey(16,{||Fa050Pesq(oMark,cAliasSE2,nIndice)})

                cSimb := Pad(Getmv("MV_SIMB"+Alltrim(STR(nMoedSubs))),4)+":"

                While .T.

                    nOpca := 0

                    //Faz o calculo automatico de dimensoes de objetos
                    oSize := FwDefSize():New(.T.)

                    oSize:lLateral := .F.
                    oSize:lProp	:= .T. // Proporcional

                    oSize:AddObject( "1STROW" ,  100, 10, .T., .T. ) // Totalmente dimensionavel
                    oSize:AddObject( "2NDROW" ,  100, 90, .T., .T. ) // Totalmente dimensionavel

                    oSize:aMargins := { 1, 1, 1, 1 } // Espaco ao lado dos objetos 0, entre eles 3

                    oSize:Process() // Dispara os calculos

                    a1stRow := {oSize:GetDimension("1STROW","LININI"),;
                    oSize:GetDimension("1STROW","COLINI"),;
                    oSize:GetDimension("1STROW","LINEND"),;
                    oSize:GetDimension("1STROW","COLEND")}

                    a2ndRow := {oSize:GetDimension("2NDROW","LININI"),;
                    oSize:GetDimension("2NDROW","COLINI"),;
                    oSize:GetDimension("2NDROW","LINEND"),;
                    oSize:GetDimension("2NDROW","COLEND")}

                    DEFINE MSDIALOG oDlg TITLE STR0020 + " - " + STR0006 From oSize:aWindSize[1],oSize:aWindSize[2] to oSize:aWindSize[3],oSize:aWindSize[4] OF oMainWnd PIXEL // "Informe Fornecedor e Loja"
                    oDlg:lMaximized := .T.

                    @ a1stRow[1], a1stRow[2] TO a1stRow[3], a1stRow[4]-2 OF oDlg  PIXEL

                    @ a1stRow[1] + 003,a1stRow[2] + 003 Say STR0017				 									PIXEL OF oDlg COLOR CLR_HBLUE // "Fornecedor : "
                    @ a1stRow[1] + 003,a1stRow[2] + 035 MSGET cCodFor F3 "FOR" Picture "@!" SIZE 70,10			  	PIXEL OF oDlg HASBUTTON

                    @ a1stRow[1] + 003,a1stRow[2] + 110 Say STR0018 												PIXEL OF oDlg COLOR CLR_HBLUE // "Loja : "
                    @ a1stRow[1] + 003,a1stRow[2] + 128 MSGET cLojaFor Picture "@!" SIZE 20,10 						PIXEL OF oDlg

                    @ a1stRow[1] + 003,a1stRow[2] + 150 Say STR0105													PIXEL OF oDlg	//"Moeda "
                    @ a1stRow[1] + 003,a1stRow[2] + 175 MSCOMBOBOX oCbx  VAR cMoeda		ITEMS aMoedas SIZE 50, 10 	PIXEL OF oDlg	ON CHANGE (nMoedSubs := Val(Substr(cMoeda,1,2)))

                    @ a1stRow[1] + 003,a1stRow[2] + 245 Say STR0106													PIXEL OF oDlg	//"Outras Moedas"
                    @ a1stRow[1] + 003,a1stRow[2] + 295 MSCOMBOBOX oCbx2 VAR cOutMoeda	ITEMS aOutMoed SIZE 60, 10	PIXEL OF oDlg

                    @ a1stRow[1] + 016,a1stRow[2] + 003 Say STR0023				PIXEL Of oDlg //"N§ T¡tulos Selecionados: "
                    @ a1stRow[1] + 016,a1stRow[2] + 245 Say STR0024+cSimb  		FONT oDlg:oFont PIXEL Of oDlg //"Valor Total: "

                    oButton := TButton():New( a1stRow[1] + 003, a1stRow[2] + 365, STR0246,oDlg,;
                                {||If(!Empty(cCodFor) .and. !Empty(cLojaFor),;
                                F050SelPR(oDlg,cOutMoeda,@nValorS,@nQtdTit,cMarca,oValor,oQtdTit,nMoedSubs,oButton,a1stRow,a2ndRow,@nRegSel,aSelFil,aTmpFil,aChaveLbn) ,;
                                HELP(" ",1,"CAMPOS OBRIGATÓRIOS",,STR0341,1, 0,,,,,, {STR0342} ))},40,15,,,.F.,.T.,.F.,,.F.,,,.F. ) //"Campos obrigatórios não foram preenchidos"###"Por favor, verifique o preenchimento dos campos Fornecedor e Loja"

                    If IsPanelFin()
                        ACTIVATE MSDIALOG oDlg ON INIT FaMyBar(oDlg,{||nOpca:=1,If(!Empty(cCodFor) .and. !Empty(cLojaFor),(nOpca := 1,oDlg:End()),HELP(" ",1,"OBRIGAT",,SPACE(45),3,0))},;
                        {||nOpca:=0,oDlg:End()},aBut050S)
                        nMoedSubs := Val(Substr(cMoeda,1,2))
                    Else
                        ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar(oDlg,{|| If(FA050VLSUB(),(nOpca := 1,oDlg:End()),NIL)},{|| nOpca := 2,oDlg:End()},,aBut050S)
                    Endif

                    Exit

                Enddo
            Else
                lSubs := .F.
            Endif
        Else
            lSubs := .T.
        Endif

        // Permitir substituir títulos normais por CC/CD e controlar baixa através do Cartão de Crédito
        If lRet .and. cPaisLoc == "EQU" .and. Len(aTitulo2CC) > 0
            Fa050Tit2CC()
            lSubs := .F.
        EndIf

        VALOR 		:= 0
        VLRINSTR 	:= 0
        IF lSubs .Or. (nQtdTit > 0 .And. nOpca == 1)
            dbSelectArea( cAlias )
            dbSetOrder(1)
            dbGoTo(nRegSel)

            nOpc := 3 //Inclusao
            lSubst:=.T.

            BEGIN TRANSACTION

                If FA050Inclu("SE2",nReg,nOpc,,,lSubst) == 1
                    lSubst := .F.
                    nValorSe2 := SE2->E2_VALOR

                    //Dados do titulo gerado (Destino)
                    If !lDelProvis
                        cPrefDest	:= SE2->E2_PREFIXO
                        cNumDest	:= SE2->E2_NUM
                        cParcDest	:= SE2->E2_PARCELA
                        cTipoDest	:= SE2->E2_TIPO
                        cCfDest		:= SE2->E2_FORNECE
                        cLojaDest	:= SE2->E2_LOJA
                        cFilDest	:= SE2->E2_FILIAL
                        dDtEmiss	:= SE2->E2_EMISSAO
                    Endif

                    // Leitura para deleção dos titulos provisórios.
                    If ( lPadrao )
                        // Inicializa Lancamento Contabil
                        nHdlPrv := HeadProva( cLote,"FINA050" /*cPrograma*/, Substr(cUsuario,7,6), @cArquivo )
                    EndIf

                    // Inicializa a gravacao dos lancamentos do SIGAPCO
                    PcoIniLan("000002")

                    //Substituicao Manual
                    If ! lF050Auto
                        aAreaSubs := __SUBS->(GetArea())
                        dbSelectArea("__SUBS")  
                        dbSetOrder(__nOrdOk)    //Ordem por __SUBS->E2_OK

                        If __SUBS->(DbSeek(cMarca))
                            __SE2->(DbGoTo(__SUBS->NUM_REG))                        

                            cFilAtu := cFilAnt

                            While __SUBS->E2_OK == cMarca

                                dbSelectArea("SE2")
                                SE2->(Dbgoto(__SUBS->NUM_REG))

                                aAdd(aNtit,	{	SE2->E2_PREFIXO,	;
                                                SE2->E2_NUM,		;
                                                SE2->E2_PARCELA,  ;
                                                SE2->E2_TIPO,     ;
                                                SE2->E2_FORNECE,  ;
                                                SE2->E2_LOJA,     ;
                                                SE2->E2_VENCREA,  ;
                                                xMoeda(SE2->E2_VALOR,SE2->E2_MOEDA,1,SE2->E2_EMISSAO),;
                                                xMoeda(SE2->E2_VALOR,SE2->E2_MOEDA,2,SE2->E2_EMISSAO),;
                                                xMoeda(SE2->E2_VALOR,SE2->E2_MOEDA,3,SE2->E2_EMISSAO),;
                                                xMoeda(SE2->E2_VALOR,SE2->E2_MOEDA,4,SE2->E2_EMISSAO),;
                                                xMoeda(SE2->E2_VALOR,SE2->E2_MOEDA,5,SE2->E2_EMISSAO),;
                                                SE2->(RECNO()),;	// [13]
                                                .F.}) // [14] -> Se integrou com PMS

                                If ( lPadrao )
                                    // Prepara Lancamento Contabil
                                    If lUsaFlag  // Armazena em aFlagCTB para atualizar no modulo Contabil
                                        aAdd( aFlagCTB, {"E2_LA", "S", "SE2", SE2->( Recno() ), 0, 0, 0} )
                                    Endif
                                    nTotal += DetProva( nHdlPrv,;
                                    cPadrao,;
                                    "FINA050" /*cPrograma*/,;
                                    cLote,;
                                    /*nLinha*/,;
                                    /*lExecuta*/,;
                                    /*cCriterio*/,;
                                    /*lRateio*/,;
                                    /*cChaveBusca*/,;
                                    /*aCT5*/,;
                                    /*lPosiciona*/,;
                                    @aFlagCTB,;
                                    /*aTabRecOri*/,;
                                    /*aDadosProva*/ )
                                EndIf

                                //Processo antigo (deletando o PR)
                                If lDelProvis
                                    // Atualizacao dos dados do Modulo SIGAPMS
                                    If IntePms()
                                        IF PmsVerAFR()
                                            aGravaAFR := PmsIncAFR()
                                        Endif
                                        lPrimeiro:= .T.
                                        PmsWriteFI(2,"SE2")	//Estorno
                                        PmsWriteFI(3,"SE2")	//Exclusao
                                    EndIf

                                    // Chama a integracao com o SIGAPCO antes de apagar o titulo
                                    PcoDetLan("000002","01","FINA050",.T.)

	                                If __lF50PROV
	                                    ExecBlock("F050PROV",.F.,.F.)
	                                Endif

                                    If lAtuSldNat .And. SE2->E2_FLUXO == 'S'
                                        AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "-",,FunName(),"SE2",SE2->(Recno()),nOpc)
                                    Endif
                                    // Exclui registros de rateio multiplas naturezas x centro de custo, no caso do titulo provisorio possuir rateio.
                                    If SE2->E2_MULTNAT == "1"
                                        FDelRatPR( "P" )
                                    Endif

                                    FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                                    Reclock("SE2",.F.,.T.)
                                    dbDelete()
                                    MsUnlock()

                                    nMaxTam := Len(aNtit)

                                    If Len(aGravaAFR) > 0 .And. (!AFR->(dbSeek(aGravaAFR[1]+aNtit[nMaxTam][1]+aNtit[nMaxTam][2]+aNtit[nMaxTam][3]+aNtit[nMaxTam][4]+aNtit[nMaxTam][5]+aNtit[nMaxTam][6])))
                                        F050GrvAFR(aGravaAFR,aNtit,nMaxTam)
                                    EndIf
                                Else

                                    If IntePms()
                                        IF PmsVerAFR()
                                            aGravaAFR := PmsIncAFR()
                                        Endif
                                        lPrimeiro:= .T. //Wilson em 06/06/2011
                                        PmsWriteFI(2,"SE2")	//Estorno
                                        PmsWriteFI(3,"SE2")	//Exclusao
                                    EndIf

                                    If lAtuSldNat .And. SE2->E2_FLUXO == 'S'
                                        AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "-",,FunName(),"SE2",SE2->(Recno()),nOpc)
                                    Endif

                                    //Processo novo (baixando o PR)
                                    lMsErroAuto := .F.

                                    cPrefOri  := SE2->E2_PREFIXO
                                    cNumOri   := SE2->E2_NUM
                                    cParcOri  := SE2->E2_PARCELA
                                    cTipoOri  := SE2->E2_TIPO
                                    cCfOri    := SE2->E2_FORNECE
                                    cLojaOri  := SE2->E2_LOJA
                                    cFilAnt   := SE2->E2_FILORIG

                                    //Baixa Provisorio
                                    aVetor 	:= {{"E2_PREFIXO"	, SE2->E2_PREFIXO 	,Nil},;
                                                {"E2_NUM"		, SE2->E2_NUM       ,Nil},;
                                                {"E2_PARCELA"	, SE2->E2_PARCELA  	,Nil},;
                                                {"E2_TIPO"	    , SE2->E2_TIPO     	,Nil},;
                                                {"E2_FORNECE"	, SE2->E2_FORNECE  	,Nil},;
                                                {"E2_LOJA"	    , SE2->E2_LOJA     	,Nil},;
                                                {"AUTMOTBX"	    , "STP"             ,Nil},;
                                                {"AUTDTBAIXA"	, dDataBase			,Nil},;
                                                {"AUTDTDEB"		, dDataBase			,Nil},;
                                                {"AUTHIST"	    , STR0330	        ,Nil}} //"Baixa ref. substituicao de titulo Provisorio para Efetivo."

                                    MSExecAuto({|x,y| Fina080(x,y)},aVetor,3)

                                    //Em caso de erro na baixa desarma a transacao
                                    If lMsErroAuto
                                        DisarmTransaction()
                                        MostraErro()
                                        Break
                                    Else
                                        //Ponto de gravação dos campos da tabela auxiliar.
                                        dbselectarea("FII")
                                        cFIISeq	 := SE5->E5_SEQ

                                        FCriaFII("SE2", cPrefOri, cNumOri, cParcOri, cTipoOri, cCfOri, cLojaOri,"SE2", cPrefDest, ;
                                                cNumDest, cParcDest, cTipoDest, cCfDest, cLojaDest, cFilDest, cFIISeq )
                                    EndIf

                                    nMaxTam := Len(aNtit)
                                    If Len(aGravaAFR) > 0 .And. (!AFR->(dbSeek(aGravaAFR[1]+aNtit[nMaxTam][1]+aNtit[nMaxTam][2]+aNtit[nMaxTam][3]+aNtit[nMaxTam][4]+aNtit[nMaxTam][5]+aNtit[nMaxTam][6])))
                                        F050GrvAFR(aGravaAFR,aNtit,nMaxTam)
                                    EndIf

                                EndIf

                                dbSelectArea("__SUBS")
                                dbSkip()
                            Enddo
                        Endif
                        
                        RestArea(aAreaSubs)

                        cFilAnt := cFilAtu

                        If IntePms()
                            PMSProjPms(aNtit) // Atualiza campo E2_PROJPMS (FUNÇÃO NO PROPRIO FINA050)
                        Endif

                    //Automatica nova
                    ElseIf Len(aItnTitPrv) > 0
                        For nI:= 1 to Len(aItnTitPrv)

                            If	(nPosPre := aScan(aItnTitPrv[nI], {|x| AllTrim(x[1]) == "E2_PREFIXO"} )) == 0 .Or.;
                                (nPosNum := aScan(aItnTitPrv[nI], {|x| AllTrim(x[1]) == "E2_NUM"    } )) == 0 .Or.;
                                (nPosPar := aScan(aItnTitPrv[nI], {|x| AllTrim(x[1]) == "E2_PARCELA"} )) == 0 .Or.;
                                (nPosTip := aScan(aItnTitPrv[nI], {|x| AllTrim(x[1]) == "E2_TIPO"   } )) == 0 .Or.;
                                (nPosFor := aScan(aItnTitPrv[nI], {|x| AllTrim(x[1]) == "E2_FORNECE"} )) == 0 .Or.;
                                (nPosLoj := aScan(aItnTitPrv[nI], {|x| AllTrim(x[1]) == "E2_LOJA"   } )) == 0

                                Loop
                            EndIf

                            SE2->(DbSetOrder(1))
                            If SE2->(MsSeek(xFilial("SE2") + aItnTitPrv[nI,nPosPre,2] + aItnTitPrv[nI,nPosNum,2] + aItnTitPrv[nI,nPosPar,2] +;
                                    aItnTitPrv[nI,nPostip,2] + aItnTitPrv[nI,nPosFor,2] + aItnTitPrv[nI,nPosLoj,2] ))

                                If ( lPadrao )
                                    // Prepara Lancamento Contabil
                                    If lUsaFlag  // Armazena em aFlagCTB para atualizar no modulo Contabil
                                        aAdd( aFlagCTB, {"E2_LA", "S", "SE2", SE2->( Recno() ), 0, 0, 0} )
                                    Endif
                                    nTotal += DetProva( nHdlPrv, cPadrao,"FINA050", cLote,/*nLinha*/,/*lExecuta*/,/*cCriterio*/,/*lRateio*/,/*cChaveBusca*/,;
                                                        /*aCT5*/,/*lPosiciona*/, @aFlagCTB,/*aTabRecOri*/,/*aDadosProva*/ )
                                EndIf

                                //Processo antigo (deletando o PR)
                                If lDelProvis

                                    If IntePms()
                                        // Atualizacao dos dados do Modulo SIGAPMS
                                        lPrimeiro:= .T. //Wilson em 06/06/2011
                                        PmsWriteFI(2,"SE2")	//Estorno
                                        PmsWriteFI(3,"SE2")	//Exclusao
                                    EndIf

                                    // Chama a integracao com o SIGAPCO antes de apagar o titulo
                                    PcoDetLan("000002","01","FINA050",.T.)

                                    If lAtuSldNat .And. SE2->E2_FLUXO == 'S'
                                        AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "-",,FunName(),"SE2",SE2->(Recno()),nOpc)
                                    Endif

                                    // Exclui registros de rateio multiplas naturezas x centro de custo, no caso
                                    // do titulo provisorio possuir rateio.
                                    If SE2->E2_MULTNAT == "1"
                                        FDelRatPR( "P" )
                                    EndIf

                                    FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                                    Reclock("SE2",.F.,.T.)
                                    dbDelete()
                                    MsUnlock()

                                    nMaxTam := Len(aNtit)
                                    If Len(aGravaAFR) > 0 .And. (!AFR->(dbSeek(aGravaAFR[1]+aNtit[nMaxTam][1]+aNtit[nMaxTam][2]+aNtit[nMaxTam][3]+aNtit[nMaxTam][4]+aNtit[nMaxTam][5]+aNtit[nMaxTam][6])))
                                        F050GrvAFR(aGravaAFR,aNtit,nMaxTam)
                                    EndIf
                                Else
                                    //Processo novo (baixando o PR)
                                    // Titulo PR será baixado na substituicao automatica
                                    lMsErroAuto := .F.

                                    cPrefOri  := SE2->E2_PREFIXO
                                    cNumOri   := SE2->E2_NUM
                                    cParcOri  := SE2->E2_PARCELA
                                    cTipoOri  := SE2->E2_TIPO
                                    cCfOri    := SE2->E2_FORNECE
                                    cLojaOri  := SE2->E2_LOJA
                                    cFilAnt   := SE2->E2_FILORIG

                                    //Baixa Provisorio
                                    aVetor 	:= {{"E2_PREFIXO"	, SE2->E2_PREFIXO 	,Nil},;
                                                {"E2_NUM"		, SE2->E2_NUM       ,Nil},;
                                                {"E2_PARCELA"	, SE2->E2_PARCELA  	,Nil},;
                                                {"E2_TIPO"	    , SE2->E2_TIPO     	,Nil},;
                                                {"E2_FORNECE"	, SE2->E2_FORNECE  	,Nil},;
                                                {"E2_LOJA"	    , SE2->E2_LOJA     	,Nil},;
                                                {"AUTMOTBX"	    , "STP"             ,Nil},;
                                                {"AUTDTBAIXA"	, dDataBase			,Nil},;
                                                {"AUTDTDEB"		, dDataBase			,Nil},;
                                                {"AUTHIST"	    , STR0330       	,Nil}}

                                    MSExecAuto({|x,y| Fina080(x,y)},aVetor,3)

                                    //Em caso de erro na baixa desarma a transacao
                                    If lMsErroAuto
                                        DisarmTransaction()
                                        MostraErro()
                                        Break
                                    Else
                                        //		Ponto de gravação dos campos da tabela auxiliar.
                                        dbselectarea("FII")
                                        cFIISeq	 := SE5->E5_SEQ

                                        FCriaFII("SE2", cPrefOri, cNumOri, cParcOri, cTipoOri, cCfOri, cLojaOri,;
                                        "SE2", cPrefDest, cNumDest, cParcDest, cTipoDest, cCfDest, cLojaDest,;
                                        cFilDest, cFIISeq )
                                    EndIf
                                EndIf
                            EndIf
                        Next nI
                    Else
                        //Automatica Antiga
                        If ( lPadrao )
                            // Prepara Lancamento Contabil
                            If lUsaFlag  // Armazena em aFlagCTB para atualizar no modulo Contabil
                                aAdd( aFlagCTB, {"E2_LA", "S", "SE2", SE2->( Recno() ), 0, 0, 0} )
                            Endif
                            nTotal += DetProva( nHdlPrv, cPadrao,"FINA050", cLote,/*nLinha*/,/*lExecuta*/,/*cCriterio*/,/*lRateio*/,/*cChaveBusca*/,;
                                                /*aCT5*/,/*lPosiciona*/, @aFlagCTB,/*aTabRecOri*/,/*aDadosProva*/ )
                        EndIf
                        dbSelectArea("SE2")
                        dbGoto(nReg)
                        If IntePms()
                            // Atualizacao dos dados do Modulo SIGAPMS
                            lPrimeiro:= .T. //Wilson em 06/06/2011
                            PmsWriteFI(2,"SE2")	//Estorno
                            PmsWriteFI(3,"SE2")	//Exclusao
                        EndIf

                        // Chama a integracao com o SIGAPCO antes de apagar o titulo
                        PcoDetLan("000002","01","FINA050",.T.)
                        If lAtuSldNat .And. SE2->E2_FLUXO == 'S'
                            AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "-",,FunName(),"SE2",SE2->(Recno()),nOpc)
                        Endif
                        // Exclui registros de rateio multiplas naturezas x centro de custo, no caso
                        // do titulo provisorio possuir rateio.
                        If SE2->E2_MULTNAT == "1"
                            FDelRatPR( "P" )
                        EndIf

                        FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                        If lDelProvis
                            Reclock("SE2",.F.,.T.)
                            dbDelete()
                            MsUnlock()
                        Else
                            If (FindFunction( "FinSubNov" ),FinSubNov(),)
                        Endif
                    Endif

                    // Finaliza a gravacao dos lancamentos do SIGAPCO
                    PcoFinLan("000002")

                    // Contabiliza a diferenca
                    dbSelectArea("SE2")
                    nRecSE2 := Recno()
                    dbGoBottom()
                    dbSkip()
                    VALOR := (nValorS - nValorSe2)
                    VLRINSTR := VALOR
                    If nTotal > 0
                        // Prepara Lancamento Contabil
                        //Contabiliza pela variavel VALOR. Nao necessita de controle de flag.
                        nTotal += DetProva( nHdlPrv, cPadrao,"FINA050", cLote,/*nLinha*/,/*lExecuta*/,/*cCriterio*/,/*lRateio*/,/*cChaveBusca*/,;
                                            /*aCT5*/,/*lPosiciona*/,/*@aFlagCTB*/,/*aTabRecOri*/,/*aDadosProva*/ )
                    EndIf
                    dbSelectArea("SE2")
                    dbGoTo(nRecSE2)
                    If nTotal > 0
                        // Envia para Lancamento Contabil
                        If  UsaSeqCor()
                            aDiario := {}
                            aDiario := {{"SE2",SE2->(recno()),SE2->E2_DIACTB,"E2_NODIA","E2_DIACTB"}}
                        Else
                            aDiario := {}
                        EndIf
                        // Efetiva Lan‡amento Contabil
                        cA100Incl( cArquivo,;
                        nHdlPrv,;
                        3 /*nOpcx*/,;
                        cLote,;
                        ( mv_par01 == 1 ) /*lDigita*/,;
                        ( mv_par07 == 1 ) /*lAglut*/,;
                        /*cOnLine*/,;
                        /*dData*/,;
                        /*dReproc*/,;
                        @aFlagCTB,;
                        /*aDadosProva*/,;
                        aDiario )
                        aFlagCTB := {}  // Limpa o coteudo apos a efetivacao do lancamento
                    Endif
                Endif

            END TRANSACTION

        Endif

        If !Empty(aChaveLbn)
            aEval(aChaveLbn, {|e| UnLockByName(e,.T.,.F.) } ) // Libera Lock
        Endif

        VALOR    := 0
        VLSINSTR := 0

        //Deleta o temporário da substituição
        If Select("__SUBS") > 0
            TcSQLExec("DELETE FROM "+__cFIN2Name)
            __SUBS->(DBGOTO(1))
        Endif
    Endif

    dbSelectArea("SE2")
    RestArea(aAreaSE2)

    For nX := 1 TO Len(aTmpFil)
        CtbTmpErase(aTmpFil[nX])
    Next

    FwFreeArray(aFlagCTB)
    FwFreeArray(aNtit)
    FwFreeArray(aGravaAFR)
    FwFreeArray(aAreaSE2)
    FwFreeArray(a1stRow)
    FwFreeArray(a2ndRow)
    FwFreeArray(aBut050S)
    FwFreeArray(aChaveLbn)
    FwFreeArray(aTitulo2CC)

Return

//-------------------------------------------------------------------
/*/{Protheus.doc} FA050Herda

Herda os dados do titulo original

@author Wagner Xavier
@since  17/02/94
@version 12
/*/
//-------------------------------------------------------------------
STATIC Function FA050Herda()

    LOCAL cAlias := Alias()
    LOCAL i
    LOCAL cCampo

    dbSelectArea("SE2")

    // Recupera os dados do titulo original
    FOR i := 1 TO FCount()
        cCampo := Field(i)
        If cCampo$"E2_PREFIXO;E2_NUM;E2_PARCELA;E2_NATUREZ;E2_FORNECE;E2_LOJA;E2_NOMFOR" .or.;
            cCampo$"E2_EMISSAO;E2_VENCTO;E2_VENCREA;E2_HISTORICO;E2_PORTADO;E2_MOEDA"
            m->&cCampo := FieldGet(i)
        EndIf
    NEXT i
    lRefresh := .T.

    dbSelectArea(cAlias)

Return

//--------------------------------------------------------------------
/*/{Protheus.doc} FA050Alter

Programa para alterção de contas a pagar

@cAlias = Alias do arquivo
@nReg = Numero do registro
@nOpc = Numero da opcao selecionada
@author Wagner Xavier
@since  27/04/92
@version 12
/*/
//---------------------------------------------------------------------
Function FA050Alter(cAlias As Character, nReg As Numeric, nOpc As Numeric) As Numeric

    Local aAlt       as Array
    LOCAL aAreaSE2   as Array
    Local aBut050    as Array
    LOCAL aCpos      as Array
    Local aCRets     as Array
    Local aFKFLoc    as Array
    Local aFKGLoc    as Array
    Local aTitImp    as Array
    LOCAL aUsers     as Array
    Local cDirfImp   as Character
    Local cDirfPai   as Character
    Local cE2ACRESC  as Character
    Local cE2DECRESC as Character
    Local cE2HIST    as Character
    Local cE2NATUREZ as Character
    Local cE2PORCJUR as Character
    Local cE2VALJUR  as Character
    Local cE2VALOR   as Character
    Local cE2VENCREA as Character
    Local cE2VENCTO  as Character
    Local cKeySE2    as Character
    Local cLojaImp   as Character
    Local cNatImp    as Character
    LOCAL cParcela   as Character
    Local cTudoOK    as Character
    Local cTxDirf    as Character
    Local lAltPA     as Logical
    Local lCalcIssBx as Logical
    Local lF050ALT   as Logical
    Local lFA050ALT  as Logical
    Local lFKF       as Logical
    Local lFKG       as Logical
    Local lFoundTx   as Logical
    Local lIRPFBaixa as Logical
    Local lJustCP    as Logical
    Local lPanelFin  as Logical
    Local lPCCBaixa  as Logical
    Local lRatPrj    as Logical
    LOCAL nIndex     as Numeric
    Local nInss      as Numeric
    LOCAL nK         as Numeric
    LOCAL nOpca      as Numeric
    Local nPosEv     as Numeric
    LOCAL nRecno     as Numeric
    Local nRecSE2    as Numeric
    Local lRet       as Logical
    Local lFaClmFKF  as Logical
    Local lFaClmFKG  as Logical
    Local aF50Clm    as Array

    Private aCols      as Array
    Private aHeader    as Array
    Private aRegs      as Array
    Private cFunct     as Character
    Private cParcIr    as Character
    Private cParcIss   as Character
    PRIVATE dEmissao   as Date
    PRIVATE dOldVencRe as Date
    PRIVATE nOldVlCruz as Numeric
    PRIVATE lFirstAlt  as Logical       
    // A variavel abaixo ira guardar valor da ultima alteracao em tela. Serve
    // p/ evitar erro na reconstituicao do valor qdoe, numa 2¦ ou n¦ altera-
    // cao, o valor do INSS for zerado.

    PRIVATE aAutRatAFR   as Array
    Private aCposAlter   as Array
    Private aCposEIC     as Array
    PRIVATE aDadosRet    as Array
    PRIVATE aRatAFR      as Array
    Private aSE2FI2      as Array
    PRIVATE bPMSDlgFI    as Block
    PRIVATE cModRetPIS   as Character
    Private cOldNatPFS   as Character
    Private cOldNaturez  as Character
    Private lAlteraTit   as Logical
    PRIVATE lAlterNat    as Logical
    PRIVATE lAltValor    as Logical
    PRIVATE lTitRetA     as Logical
    Private _Opc         as Numeric
    Private nBtrISSOri   as numeric
    Private nCofInter    as Numeric
    Private nCofOri      as Numeric
    Private nCslInter    as Numeric
    Private nCslOri      as Numeric
    Private nIrfOri      as Numeric
    Private nISSOri      as Numeric
    PRIVATE nOldVlAcres  as Numeric
    PRIVATE nOldVlDecres as Numeric
    Private nPisInter    as Numeric
    Private nPisOri      as Numeric
    PRIVATE nVlAltInss   as Numeric
    PRIVATE nVlAltSEST   as Numeric
    Private nVlrOri      as Numeric
    // Utilizado para avaliar alteração *
    // no vencimento real               *
    Private dVencReaAnt	 as Date
    // Utilizado para armazemar valor *
    // alterado na tela de alteração  *
    Private cDirfAlt as Character
    Private lRatOk   as Logical

    Default __lMetric  := FwLibVersion() >= "20210517"
    Default __lFA50UPD := ExistBlock("FA050UPD")
    Default __lFNCDRET := ExistBlock("FINCDRET")
    Default __lPLSFN50 := FindFunction("PLSFN050")
    Default __lHasEAI  := FWHasEAI("FINA050", .T.,, .T.)
    Default __lTemMR   := (FindFunction("FTemMotor") .and. FTemMotor())
    Default __lIntPFS  := SuperGetMv("MV_JURXFIN",.T.,.F.) //Integração do Financeiro com o Juridico(Habilitado = .T.)
    Default __lFnBtr   := FindFunction("ISSCPOM") .And. FindFunction("BtrISSMun")
    Default __lBtrISS  := SE2->(ColumnPos("E2_BTRISS")) > 0 .And. SE2->(ColumnPos("E2_VRETBIS")) > 0 .And. SE2->(ColumnPos("E2_CODSERV")) > 0 .And. __lFnBtr


    lPanelFin := IsPanelFin()
    nOpca     := 0
    aCpos     := {}
    nRecno    := 0
    cParcela  := E2_PARCELA
    nK        := 0
    aUsers 	  := {}
    aAreaSE2  := SE2->(GetArea())
    nIndex	  := SE2->(IndexOrd())
    cTudoOK   := Nil
    aBut050	  := {}

    //Controla o Pis Cofins e Csll na baixa
    lPCCBaixa     := SuperGetMv("MV_BX10925",.T.,"2") == "1"

    lIRPFBaixa    := .F.
    nInss         := SE2->E2_INSS
    lCalcIssBx    := IsIssBx("P")
    lJustCP       := CposJust()
    cDirfImp      := ""
    cDirfPai      := ""
    lFoundTx      := .F.
    cLojaImp      := PadR( "00", TamSX3("A2_LOJA")[1], "0" )
    aCRets        := {}
    lRatPrj       :=.T.//indica se existe rateio de projetos
    cE2NATUREZ    := Alltrim(SE2->E2_NATUREZ)
    cE2VENCTO     := DTOC(SE2->E2_VENCTO)
    cE2VENCREA    := DTOC(SE2->E2_VENCREA)
    cE2VALOR      := Alltrim(Transform(SE2->E2_VALOR,PesqPict("SE2","E2_VALOR")))
    cE2DECRESC    := Alltrim(Transform(SE2->E2_DECRESC,PesqPict("SE2","E2_DECRESC")))
    cE2ACRESC     := Alltrim(Transform(SE2->E2_ACRESC,PesqPict("SE2","E2_ACRESC")))
    cE2VALJUR     := Alltrim(Transform(SE2->E2_VALJUR,PesqPict("SE2","E2_VALJUR")))
    cE2PORCJUR    := Alltrim(Transform(SE2->E2_PORCJUR,PesqPict("SE2","E2_PORCJUR")))
    cE2HIST       := Alltrim(SE2->E2_HIST)
    aAlt          := {}
    cKeySE2 	  := SE2->(indexkey())
    nRecSE2 	  := SE2->(Recno())
    lFA050ALT     := ExistBlock("FA050ALT")
    lF050ALT	  := ExistBlock("F050ALT")
    lAltPA        := .F.
    cTxDirf	      := ""
    lFKG          := .F.
    aFKGLoc       := {}
    nPosEv        := 0
    lFKF          := .F.
    aFKFLoc       := {}
    aTitImp       := {}
    cNatImp       := ""

    aHeader       := {}
    aCols         := {}
    aRegs         := {}
    cParcIr       := ""
    cFunct		  := ""
    cParcIss      := ""
    dOldVencRe	  := SE2->E2_VENCREA
    nOldVlCruz	  := SE2->E2_VLCRUZ
    dEmissao 	  := SE2->E2_EMISSAO
    lFirstAlt     := .T.
    // A variavel abaixo ira guardar valor da ultima alteracao em tela. Serve
    // p/ evitar erro na reconstituicao do valor qdoe, numa 2¦ ou n¦ altera-
    // cao, o valor do INSS for zerado.
    nVlAltInss	 := 0
    nVlAltSEST   := 0
    aRatAFR		 := {}
    bPMSDlgFI	 := {||PmsDlgFI(4,M->E2_PREFIXO,M->E2_NUM,M->E2_PARCELA,M->E2_TIPO,M->E2_FORNECE,M->E2_LOJA)}
    aAutRatAFR	 := {}
    nOldVlAcres  := SE2->E2_ACRESC
    nOldVlDecres := SE2->E2_DECRESC
    cModRetPIS   := GetNewPar( "MV_RT10925", "1" )
    aDadosRet    := Array(5)
    lAlterNat    := .F.
    nVlrOri      := SE2->E2_VALOR
    nPisOri      := SE2->E2_PIS
    nCofOri      := SE2->E2_COFINS
    nCslOri      := SE2->E2_CSLL
    nIrfOri      := SE2->E2_IRRF
    nISSOri      := SE2->E2_ISS
    nBtrISSOri   := 0
    nPisInter    := SE2->E2_PIS
    nCofInter    := SE2->E2_COFINS
    nCslInter    := SE2->E2_CSLL
    cOldNaturez  := SE2->E2_NATUREZ
    cOldNatPFS   := SE2->E2_NATUREZ
    aCposAlter   := {}
    _Opc         := nOpc
    aSE2FI2		 :=	{} // Utilizada para gravacao das justificativas
    lAltValor 	 := .F.
    lTitRetA 	 := .F.
    lAlteraTit   := .F. //DFS - 06/08/13 - Inclusão de flag para permitir apenas alterar o vencimento da Nota Fiscal gerada a partir do módulo EIC
    aCposEIC     := {}  //LGS - 18/05/16 - Utilizado no tratamento de validações para titulos originados pelo sigaeic
    // Utilizado para avaliar alteração *
    // no vencimento real               *
    dVencReaAnt	 := SE2->E2_VENCREA
    // Utilizado para armazemar valor *
    // alterado na tela de alteração  *
    cDirfAlt := ""
    lRatOk   := .T.
    lRet     := .T.
    lFaClmFKF	:= ExistBlock( "FACLMFKF" )
    lFaClmFKG   := ExistBlock( "FACLMFKG" )
    aF50Clm     := {}    

    SE2->(DbSetOrder(nIndex))
    If !(SE2->(MsSeek(SE2->(&(cKeySE2)))))
        Help(" ",1,"ARQVAZIO")
        Return .T.
    Else
        SE2->(dbGoto(nRecSE2))
    Endif

    __cFunBkp := FunName()
    __cFunMet := Iif(AllTrim(__cFunBkp)=='RPC',"RPCFINA050",__cFunBkp)

    If __lMetric
        SetFunName(__cFunMet)
        // Metrica de controle de acessos 
        FwCustomMetrics():setSumMetric(Alltrim(ProcName()), "financeiro-protheus_qtd-por-acesso_total", 1)
        SetFunName(__cFunBkp)
    Endif

    nOldTxMoeda	:= 0

    If __lBtrISS
        nBtrISSOri := SE2->E2_BTRISS
    EndIf

    SA2->(dbSetOrder(1))
    SA2->(MSSeek(xFilial("SA2")+SE2->(E2_FORNECE+E2_LOJA)))

    //Motor de retenções,verifica quais impostos estão configurados
    If __lTemMR
        F050ImpCon(4)
    EndIf

    lIRProg := IIf(__lLocBRA,IIf(!Empty(SA2->A2_IRPROG),SA2->A2_IRPROG,"2"),"2")

    lIRPFBaixa := IIf( __lLocBRA, SA2->A2_CALCIRF == "2", .F.)

    lF050Auto   := IF(Type("lF050Auto") == "U", .F., lF050Auto)

    __lRateioIR := .F.

    //Permite rateio de Múltiplas natureza
    __lRatMNat := MV_MULNATP .And. Empty(SE2->E2_BAIXA) .And. AllTrim(SE2->E2_LA) <> "S" .And.;
    SE2->(E2_VALOR == E2_SALDO) .And. Alltrim(SE2->E2_ORIGEM) <> "MATA100"

    IF SED->ED_DEDINSS == "2"  //Nao desconta o INSS do principal
        nInss := 0
    Endif

    //Verifica se o título gera ou nao DIRF, buscando essa informação nos TXs.
    If __lLocBRA .And. !(Alltrim(SE2->E2_TIPO) $ MVTAXA+"|"+MVTXA)
        nRecno   := SE2->(Recno())
        cChave   := SE2->E2_FILIAL+SE2->E2_PREFIXO+SE2->E2_NUM
        cDirfPai := SE2->E2_DIRF
        cTxDirf := If(lAltPa,MVTXA,MVTAXA)
        cNatImp := AllTrim(GetMv("MV_PISNAT"))+"|"+AllTrim(GetMv("MV_COFINS"))+"|"+AllTrim(GetMv("MV_CSLL")) + "|"+ AllTrim(&(GetMv("MV_IRF")))
       
         aTitImp := ImpCtaPg(/*cImposto*/ , .F. )
        For nK := 1 To Len(aTitImp)

            If AllTrim(aTitImp[nK][4]) ==  cTxDirf  .And. AllTrim(aTitImp[nK][5]) $ cNatImp
                cDirfImp := SE2->E2_DIRF
                Exit
            Endif
        Next nK

        SE2->(dbGoto(nRecno))
        cDirfImp := If(AllTrim(cDirfImp) <> "", cDirfImp, cDirfPai)
        //Atualizo o campo E2_DIRF com o valor preenchido inicialmente, somente para a tela de alteração.
        If SE2->E2_DIRF<>cDirfImp
            RecLock("SE2",.F.)
            SE2->E2_DIRF := cDirfImp
            MsUnlock()
        EndIf
    Endif

    // Codigo de retenção anterior para IN4815
    cOldCodRet	:= SE2->E2_CODRET
    nOldIrrf 	:= SE2->E2_IRRF
    nOldIssInt 	:= SE2->E2_ISS
    nOldValor	:= SE2->E2_VALOR
    nOldSaldo	:= SE2->E2_SALDO
    nOldIns	 	:= SE2->E2_INSS
    nOldSES     := SE2->E2_SEST
    nValorAnt 	:= SE2->E2_VALOR
    If __lLocBRA
        nOldCID		:= SE2->E2_CIDE
    EndIf
    nOldPisAnt	:= SE2->E2_PIS
    nOldCofAnt	:= SE2->E2_COFINS
    nOldCslAnt	:= SE2->E2_CSLL
    If !lF050Auto
        aDadosRet := Array(5)
        nVlRetPis	:= 0
        nVlRetCof	:= 0
        nVlRetCsl	:= 0
        Afill(aDadosRet,0)
    Endif

    //Se controla Retencao
    If !lPccBaixa
        nOldPisAnt := IIF(Empty(SE2->E2_VRETPIS), SE2->E2_PIS , SE2->E2_VRETPIS )
        nOldCofAnt := IIF(Empty(SE2->E2_VRETCOF), SE2->E2_COFINS , SE2->E2_VRETCOF )
        nOldCslAnt := IIF(Empty(SE2->E2_VRETCSL), SE2->E2_CSLL , SE2->E2_VRETCSL )
    Endif

    // Grava o valor que realmente foi retido nos campos do PCC  *
    // para ser apresentado na tela do AxAltera e não afetar os  *
    // titulos de PCC gerados na emissão.                        *
    If __lLocBRA .and. !lPccBaixa .and. !lAlterNat
        //Gravo temporariamente do PIS/Cofins/Csll.
        RECLOCK("SE2",.F.,,.T.)
        SE2->E2_PIS := nOldPisAnt
        SE2->E2_COFINS := nOldCofAnt
        SE2->E2_CSLL := nOldCslAnt
        MsUnlock()
    EndIf

    //Botoes adicionais na EnchoiceBar
    aBut050 := fa050BAR('IntePms()')

    //inclusao do botao Posicao
    AADD(aBut050, {"HISTORIC", {|| Fc050Con() }, STR0204}) //"Posicao"

    //inclusao do botao Rastreamento
    AADD(aBut050, {"HISTORIC", {|| Fin250Pag(2) }, STR0205}) //"Rastreamento"

    If lJustCP // Adiciona botao para justificativa
        Aadd(aBut050,{'BAIXATIT',{||Fa050JUST()},STR0134})		//"Justificativa"
    Endif

    If __lIntPFS .And. FindFunction("JURA246") .And. !(SE2->E2_TIPO $ MVTAXA+"|"+MVINSS+"|"+MVISS+"|"+MVTXA+"|SES|INA|IRF|PIS|COF|CSL")
        Aadd(aBut050,{"", {|| JURA246(4) }, STR0296}) //"Detalhe / Desdobramentos" (Modulo SIGAPFS)
    EndIf

    // Somente permite a alteracao de multiplas naturezas para titulo digitados
    If ((SE2->E2_MULTNAT == "1" .And. Alltrim(SE2->E2_ORIGEM) <> "MATA100") .OR. __lRatMNat) .And.;
        SE2->E2_FORNECE + SE2->E2_LOJA # GetMV("MV_UNIAO"  ) + Space(Len(SE2->E2_FORNECE) - Len(GetMV("MV_UNIAO"  ))) + cLojaImp .And.;
        SE2->E2_FORNECE + SE2->E2_LOJA # GetMV("MV_FORINSS") + Space(Len(SE2->E2_FORNECE) - Len(GetMV("MV_FORINSS"))) + cLojaImp .And.;
        SE2->E2_FORNECE + SE2->E2_LOJA # GetMV("MV_MUNIC"  ) + Space(Len(SE2->E2_FORNECE) - Len(GetMV("MV_MUNIC"  ))) + cLojaImp

        Aadd(aBut050, {'S4WB013N',;
        {||	MultNat(	"SE2",;
        0 /*@nHdlPrv*/,;
        M->E2_VALOR /*@nTotal*/,;
        "", /*@cArquivo*/;
        .F. /*lContabiliza*/,;
        If( SE2->E2_LA != "S", 4, 2 ) /*nOpc*/,;
        If(	/*lExpr*/	mv_par06==1,;
        /*T*/	If(	lPccBaixa .Or. ( lIRPFBaixa .And. ! M->E2_TIPO $ MVPAGANT ),;
        0,;
        M->E2_IRRF ) +;
        If( !lCalcIssBx, M->E2_ISS, 0 ) +;
        nInss +;
        M->E2_RETENC +;
        M->E2_SEST +;
        If( lPccBaixa, 0, E2_PIS + E2_COFINS + E2_CSLL ),;
        /*F*/	0 ) /*nImpostos*/,;
        mv_par10 = 2 .And. mv_par06 = 2 /*lRatImpostos*/,;
        aHeader /*acolsM*/,;
        aCols /*aHeaderM*/,;
        aRegs /*aRegs*/,;
        .F. /*lGrava*/,;
        /*lMostraTela*/,;
        /*lRotAuto*/,;
        /*lUdaFlag*/,;
        /*@aFlagCTB*/) },;
        STR0116 /*Rateio das Naturezas do titulo*/,;
        STR0123 /*Rateio*/ } )
    Endif

    If SE2->( EOF()) .or. xFilial("SE2") # SE2->E2_FILIAL
        Help(" ",1,"ARQVAZIO")
        Return .T.
    Endif

    ///Projeto
    // Verifica campos do usuario
    aUsers := FinLoadSX3("SE2",{|cField| GetSX3Cache(cField,"X3_PROPRI") == "U"},{{"X3_CAMPO",nil}})

    // Validação Siafi
    If FinTemDH()
        Return .T.
    Endif

    //Se veio atraves da integracao Protheus X Tin nao Pode ser alterado
    If (!Type("lF050Auto") == "L" .Or. !lF050Auto) .and.  Upper(AllTrim(SE2->E2_ORIGEM))=="FINI055"
        HELP(" ",1,"ProtheusXTIN",,STR0213,2,0)//"Título gerado pela Integração Protheus X Tin não Pode ser alterado pelo Protheus"
        Return
    Endif

    // AAF - Titulos originados no SIGAEFF não devem ser alterados
    If !lF050Auto .AND. "SIGAEFF" $ SE2->E2_ORIGEM
        Help(" ",1,"FAORIEFF")
        Return
    EndIf

    //verifica se e titulo originado do SIGAPLS e nao deixa alterar.
    if __lPLSFN50 .and. ! lF050Auto .and. PLSFN050(nOpc)
        return(.f.)
    endIf
    
    // usa o Modulo 88 GTP
    If nModulo <> 88
        If  Upper(substr(SE2->E2_ORIGEM,1,7)) $ IIF(FindFunction('GTPFUNCRET'),GTPFUNCRET('FINA050','2','SE2'),'GTPA421|GTPA700|GTPA700A|GTPA700L|GTPA819')
            Help(" ",1,"NODELGTP",,STR0372,1,0) //"Este título não pode ser alterado ou cancelada sua baixa ,pois foi gerado através do GTP."
            Return
        EndIf
    EndIf

    // DFS - 16/03/11 - Deve-se verificar se os títulos foram gerados por módulos Trade-Easy, antes de apresentar a mensagem.
    // TDF - 26/12/11 - Acrescentado o módulo EFF para permitir liquidação
    // NCF - 25/03/13 - Acrescentado o módulo SIGAESS (Siscoserv)
    If (UPPER(Alltrim(SE2->E2_ORIGEM)) $ "SIGAEEC/SIGAEIC/SIGAEDC/SIGAECO/SIGAESS" .OR.;
    (!(Left(Alltrim(SE2->E2_ORIGEM),3) == 'FIN') .And. SE2->E2_PREFIXO == 'EIC')) .AND. !(cModulo $ "EEC/EIC/EDC/ECO/EFF/ESS")

        If FindFunction("EasyOrigem")
            If F050EasyOrig(AllTrim(SE2->E2_ORIGEM))
                If lAlteraTit
                    aCpos := aClone( aCposEIC )
                Else
                    Return
                EndIf
            EndIf
        Else
            If Posicione("SA2",1,xFilial("SA2")+SE2->(E2_FORNECE+E2_LOJA),"A2_PAIS") <> "105" .AND. SE2->E2_MOEDA > 1
                HELP(" ",1,"FAORIEEC")
                Return
                // GFP - 07/03/2014 - Tratamento para liberar os campos que são permitidos para alteração, com exceção daqueles utilizados pelos módulos de Comercio Exterior.
            ElseIf UPPER(Alltrim(SE2->E2_TIPO)) == "NF" .AND. SE2->E2_MOEDA == 1
                aCpos := fa050MCpo(4)
                If (nPos := aScan(aCpos, "E2_VENCREA")) # 0
                    ADEL(aCpos,nPos)
                    ASIZE(aCpos,LEN(aCpos)-1)
                EndIf
                If (nPos := aScan(aCpos, "E2_VALOR")) # 0
                    ADEL(aCpos,nPos)
                    ASIZE(aCpos,LEN(aCpos)-1)
                EndIf
                If (nPos := aScan(aCpos, "E2_VLCRUZ")) # 0
                    ADEL(aCpos,nPos)
                    ASIZE(aCpos,LEN(aCpos)-1)
                EndIf
                lAlteraTit := .T.
            Else   
                HELP(" ",1,"FAORIEEC")
                Return
            EndIf
        EndIf
    Endif

    // Caso titulo esteja num bordero nao pode sofrer alteracao
    If !Empty(SE2->E2_NUMBOR)
        Help(" ",1,"F050BORD",,STR0099+CHR(13)+STR0100,1,0)
        Return
    EndIf

    // Verifica se o titulo esta bloqueado
    If !Empty(SE2->(FieldPos("E2_MSBLQL"))) .And. SE2->E2_MSBLQL == "1" .And. lVerifyBlq .and. UPPER(Alltrim(SE2->E2_ORIGEM)) $ "CNTA090/CNTA100/CNTA120/CNTA121"
        Help(" ",1,"SE2BLOQ")
        Return
    EndIf

    // Verifica se data do movimento n„o ‚ menor que data limite de
    // movimentacao no financeiro
    If !DtMovFin(,,"1")
        Return
    Endif

    nOldIRR 	:= SE2->E2_IRRF
    nOldISS 	:= SE2->E2_ISS
    If __lBtrISS
        nOldBtrISS := SE2->E2_BTRISS
    EndIf
    nOldInss	:= SE2->E2_INSS
    nOldSEST	:= SE2->E2_SEST
    If __lLocBRA
        nOldCID  := SE2->E2_CIDE
    EndIf

    // Se existir os campos de impostos a pagar, PIS, COFINS, CSLL - MP 135
    If !lPccBaixa
        nOldPis	   := SE2->E2_PIS
        nOldCofins := SE2->E2_COFINS
        nOldCsll   := SE2->E2_CSLL
    Endif
    // Atencao para criar o array aCpos
    cParcIss	 := If(Empty(SE2->E2_PARCISS),cParcela,SE2->E2_PARCISS)
    cParcIr	 := If(Empty(SE2->E2_PARCIR ),cParcela,SE2->E2_PARCIR )
    cParcInss := If(Empty(SE2->E2_PARCINS),cParcela,SE2->E2_PARCINS)
    cParcSEST := If(Empty(SE2->E2_PARCSES),cParcela,SE2->E2_PARCSES)
    If __lLocBRA
        cParcCIDE := If(Empty(SE2->E2_PARCCID),cParcela,SE2->E2_PARCCID)
        __lFlagFKF := FKF->(ColumnPos("FKF_REINF")) > 0  // Monta lista HashMap com informações do título referentes ao EFD REINF
    EndIf
    
    If __lFlagFKF
        __oHsREINF := tHashMap():New()
        __oHsREINF:Set("_VALTIT",SE2->E2_VALOR)
        __oHsREINF:Set("_NATFIN",SE2->E2_NATUREZ)
        __oHsREINF:Set("_ALTCOMPL",.F.)
    EndIf

    //Monta campos para usuario
    //DFS - 06/08/13 - Caso não seja alteração de título gerado pelo EIC, pode incluir os outros campos para alteração
    If !lAlteraTit
        aCpos := fa050MCpo(4)
    EndIf
    If aCpos == Nil
        return
    EndIf

    // Caso seja um PA, somente permite alterar o historico e campos de usuario
    If SE2->E2_TIPO $ MVPAGANT .And. F050MovBco()
        lAltPA := .T.
        aCpos := {"E2_HIST"}
    EndIf

    aCposAlter := aClone( aCpos )

    // Preenche campos alter veis (usu rio)
    If Len(aUsers) > 0
        FOR nk:=1 TO Len(aUsers)
            Aadd(aCpos,Alltrim(aUsers[nk]))
        NEXT nk
    EndIf

    lAltera := .T.

    dbSelectArea("SA2")
    dbSeek(cFilial+SE2->E2_FORNECE+SE2->E2_LOJA)

    dbSelectArea( cAlias )
    dbSetOrder(1)

    IF __lFA50UPD
        // Ponto de Entrada para Pre-Validacao de Alteracao
        IF !ExecBlock("FA050UPD",.f.,.f.)
            Return .F.
        Endif
    Endif

    // integração com o PMS
    If IntePMS()
        SetKey(VK_F10, {|| Eval(bPMSDlgFI)})
    EndIf
    
    If __lFa986Vld == NIL
	    __lFa986Vld := FindFunction( "F986Vld" )
    Endif

    cTudoOk := 'Iif(Len(aSE2FI2)==0,Fa050JUST(),.T.) .And. F050PcoLan() '
    
    If __lLocBRA .And. __lFa986Vld
        cTudoOk += '.And. F986Vld("SE2")'
    Endif

    If !lF050Auto
        cTudoOk += ' .And. If(M->E2_TEMDOCS == "1",CN062NecDocs(),.T.) ' //Documentos
        cTudoOk += ' .And. F050CodRet()'
    EndIf

    IF lFA050ALT
        cTudoOK += ' .and. ExecBlock("FA050ALT",.f.,.f.)'
    Endif
    If  IntePMS() .and. (nPosAFR:=AScan(aAutocab,{|x|AllTrim(x[1])=="AUTRATAFR"})) >0 //rateio automatico de projetos
        aAutoAFR:=aClone(aAutoCab[nPosAFR][2])
        cTudoOk+=' .and. F050AutAFR('+Str(nOpc,2)+') '
    Endif

    cTudoOK += ' .And. F050VldVlr() '

    If FindFunction("JurValidCP") .And. __lIntPFS
        cTudoOK += ' .And. JurValidCP(4) '
    EndIf

    cTudoOK += ' .and. f050RatOk(lRatOK) .And. F986NatRen() '

    Afill(aDadosRet,0)
    //Controle de retencao do PIS/Cofins/CSLL
    If __lLocBRA .and. !lPccBaixa .and. !lAltPA

        If (SE2->E2_PRETPIS == "1" .or. SE2->E2_PRETCOF == "1" .or. SE2->E2_PRETCSL == "1" )
            nOldPis := 0
            nOldCofins := 0
            nOldCsll := 0
        Else
            nOldPis := IIF(Empty(SE2->E2_VRETPIS), SE2->E2_PIS , SE2->E2_VRETPIS )
            nOldCofins := IIF(Empty(SE2->E2_VRETCOF), SE2->E2_COFINS , SE2->E2_VRETCOF )
            nOldCsll := IIF(Empty(SE2->E2_VRETCSL), SE2->E2_CSLL , SE2->E2_VRETCSL )
        Endif

        //Apresento o botao de retencao apenas se existir possibilidade de alteracao do valor
        If (nPos := Ascan(aCpos,{ |x| x == "E2_VALOR" } )) > 0
            Aadd(aBut050,{"NOTE",{||F050CalcRt()},STR0125,STR0126})  //"Modalidade de Retenção Pis/Cofins/Csll"###"Impostos"
        Endif
    EndIf

    ///Projeto
    // Inicializa a gravacao dos lancamentos do SIGAPCO
    PcoIniLan("000002")

    If lAltPA
        cFunct :=""
        aBut050 := {}
    Else
        cFunct :="FA050AXALT('"+cAlias+"','"+cParcIss+"','"+cParcIr+"','"+cParcInss+"','"+cParcSEST+"')"
    EndIf

    Begin Transaction
        __nVlrMR    := 0
        If !Type("lF050Auto") == "L" .or. !lF050Auto
            If lPanelFin  //Chamado pelo Painel Financeiro
                dbSelectArea("SE2")
                RegToMemory("SE2",.F.,.F.,.F.,FunName())
                nValDig := M->E2_VALOR	// Carrega o valor do titulo para nao zerar variavel de memoria no uso de gatilho
                oPanelDados := FinWindow:GetVisPanel()
                oPanelDados:FreeChildren()
                aDim := DLGinPANEL(oPanelDados)
                nOpca := AxAltera(cAlias,nReg,nOpc,,aCpos,4,SA2->A2_NOME,cTudoOk,cFunct,,aBut050,/*aParam*/,/*aAuto*/,/*lVirtual*/,/*lMaximized*/,/*cTela*/,.T.,oPanelDados,aDim,FinWindow)

            Else
                RegToMemory("SE2",.F.,.F.,.F.,FunName()) // incluido Eduardo
                nValDig := M->E2_VALOR	// Carrega o valor do titulo para nao zerar variavel de memoria no uso de gatilho
                nOpca := AxAltera(cAlias,nReg,nOpc,,aCpos,4,SA2->A2_NOME,cTudoOk,cFunct,,aBut050)
            Endif
        Else
            RegToMemory("SE2",.F.,.F.)
            nValDig := M->E2_VALOR
            If f050AltCmp(aCpos, aAutoCab) .And. EnchAuto(cAlias,aAutoCab,cTudoOk,nOpc )
                nPosEv := AScan(aAutocab,{|x|AllTrim(x[1])=="AUTCMTIT"})
                If nPosEv>0
                    aFKFLoc := aClone(aAutoCab[nPosEv][2])
                    lFKF    := .T.
                EndIf

                nPosEv:=AScan(aAutocab,{|x|AllTrim(x[1])=="AUTCMIMP"})
                If nPosEv>0
                    aFKGLoc := aClone(aAutoCab[nPosEv][2])
                    lFKG    := .T.
                EndIf
                
                if lFaClmFKF
                   aF50Clm := ExecBlock("FACLMFKF",.F.,.F.,{aFKFLoc,"SE2",4})               
                    if Valtype(aF50Clm) == "A"
                        If !Empty(aF50Clm)
                            aFKFLoc := ACLONE(aF50Clm)
                            lFKF := .T.
                        EndIf 
                    endif 

                    aF50Clm := {}
                endif    

                if lFaClmFKG 
                   aF50Clm := ExecBlock("FACLMFKG",.F.,.F.,{aFKGLoc,"SE2",4})
                    if Valtype(aF50Clm) == "A"
                        If !Empty(aF50Clm)
                            aFKGLoc := ACLONE(aF50Clm)
                            lFKG:= .T.
                        EndIf   
                    endif 
                
                    aF50Clm := {}
                endif

                If cPaisLoc=="BRA" .and. (lFKF .or. lFKG)
                    lRet:= F986ExAut("SE2", aFKFLoc, aFKGLoc, 4, aAutocab)
                EndIf
                If lRet
                    nOpcA := AxIncluiAuto(cAlias,,cFunct,4,SE2->(RecNo()))
                EndIf
            EndIf
        EndIf

        IF lF050ALT .and. !lAltPA
            // Ponto de Entrada para Valida‡Æo pos-Confirma‡Æo de Alteracao
            ExecBlock("F050ALT",.f.,.f.,{nOpca})
        Endif

        If nOpca == 1 //verifica se houve alterações, para geração do log de alterações

            If !(cE2NATUREZ == Alltrim(SE2->E2_NATUREZ))
                aadd( aAlt,{ STR0253 , STR0254 + ' :', STR0255 + ' - '  + STR0258 , STR0256 + ' - ' +  Alltrim(cE2NATUREZ) , STR0257 + ' - ' + Alltrim(SE2->E2_NATUREZ)})
            endif

            If !(cE2VENCTO == Alltrim(DTOC(SE2->E2_VENCTO)))
                aadd( aAlt,{ STR0253 , STR0254 + ' :', STR0255 + ' - '  + STR0259 , STR0256 + ' - ' + Alltrim(cE2VENCTO) , STR0257 + ' - ' +  Alltrim(DTOC(SE2->E2_VENCTO))})
            endif

            If !(cE2VENCREA == Alltrim(DTOC(SE2->E2_VENCREA)))
                aadd( aAlt,{ STR0253 , STR0254 + ' :', STR0255 + ' - '  + STR0260 , STR0256 + ' - '  +  Alltrim(cE2VENCREA) , STR0257 + ' - ' +  Alltrim(DTOC(SE2->E2_VENCREA))})
            endif

            If !(cE2VALOR == Alltrim(Transform(SE2->E2_VALOR,PesqPict("SE2","E2_VALOR"))))
                aadd( aAlt,{ STR0253 , STR0254 + ' :', STR0255 + ' - '  + STR0261 , STR0256 + ' - '  +  Alltrim(cE2VALOR) , STR0257 + ' - ' + Alltrim(Transform(SE2->E2_VALOR,PesqPict("SE2","E2_VALOR"))) })
            endif

            If !(cE2DECRESC == Alltrim(Transform(SE2->E2_DECRESC,PesqPict("SE2","E2_DECRESC"))))
                aadd( aAlt,{ STR0253 , STR0254 + ' :', STR0255 + ' - '  + STR0262 , STR0256 + ' - '  +  Alltrim(cE2DECRESC) ,STR0257 + ' - ' + Alltrim(Transform(SE2->E2_DECRESC,PesqPict("SE2","E2_DECRESC"))) })
            endif

            If !(cE2ACRESC == Alltrim(Transform(SE2->E2_ACRESC,PesqPict("SE2","E2_ACRESC"))))
                aadd( aAlt,{ STR0253 , STR0254 + ' :', STR0255 + ' - '  + STR0263 , STR0256 + ' - '  +  Alltrim(cE2ACRESC) ,STR0257 + ' - ' + Alltrim(Transform(SE2->E2_ACRESC,PesqPict("SE2","E2_ACRESC"))) })
            endif

            If !(cE2VALJUR == Alltrim(Transform(SE2->E2_VALJUR,PesqPict("SE2","E2_VALJUR"))))
                aadd( aAlt,{ STR0253 , STR0254 + ' :', STR0255 + ' - '  + STR0264 , STR0256 + ' - ' +  Alltrim(cE2VALJUR) , STR0257 + ' - ' +  Alltrim(Transform(SE2->E2_VALJUR,PesqPict("SE2","E2_VALJUR"))) })
            endif

            If !(cE2PORCJUR == Alltrim(Transform(SE2->E2_PORCJUR,PesqPict("SE2","E2_PORCJUR"))))
                aadd( aAlt,{ STR0253 , STR0254 + ' :', STR0255 + ' - '  + STR0265 , STR0256 + ' - '  +  Alltrim(cE2PORCJUR) ,STR0257 + ' - ' +  Alltrim(Transform(SE2->E2_PORCJUR,PesqPict("SE2","E2_PORCJUR"))) })
            endif

            If !(cE2HIST == Alltrim(SE2->E2_HIST))
                aadd( aAlt,{ STR0253 , STR0254 + ' :', STR0255 + ' - '  + STR0266 , STR0256 + ' - '  +  Alltrim(cE2HIST) ,STR0257 + ' - ' +  Alltrim(SE2->E2_HIST)})
            endif

            ///chamada da Função que cria o Log de alterações
            FinaCONC(aAlt,"SE2")

        endif

        If __lLocBRA .and. !lPccBaixa .and. nOpca != 1 .and.(!lAlterNat .or. ;
        STR(SE2->E2_PIS+SE2->E2_COFINS+SE2->E2_CSLL,17,2) == STR(nPisOri+nCofOri+nCslOri,17,2))
            //Regravo os valores originais de PIS/Cofins/Csll em caso de desistencia de alteracao
            RECLOCK("SE2",.F.,,.T.)
            SE2->E2_PIS := nPisOri
            SE2->E2_COFINS := nCofOri
            SE2->E2_CSLL := nCslOri
            MsUnlock()
        EndIf

        nOldValor := SE2->E2_VALOR
        nOldSaldo := SE2->E2_SALDO
        nOldIRR   := SE2->E2_IRRF
        nOldISS   := SE2->E2_ISS
        If __lBtrISS
            nOldBtrISS := SE2->E2_BTRISS
        EndIf
        nOldInss  := SE2->E2_INSS
        nOldSEST  := SE2->E2_SEST
        nOldPis	  := SE2->E2_PIS
        nOldCofins:= SE2->E2_COFINS
        nOldCsll  := SE2->E2_CSLL

        If !lPccBaixa
            nOldPis := IIF(Empty(SE2->E2_VRETPIS), SE2->E2_PIS , SE2->E2_VRETPIS )
            nOldCofins := IIF(Empty(SE2->E2_VRETCOF), SE2->E2_COFINS , SE2->E2_VRETCOF )
            nOldCsll := IIF(Empty(SE2->E2_VRETCSL), SE2->E2_CSLL , SE2->E2_VRETCSL )
        Endif
        // Finaliza a gravacao dos lancamentos do SIGAPCO
        PcoFinLan("000002")

        PcoFreeBlq("000002")

        // Trexo que altera os campos Gera Dirf e Codigo de Retencao
        // dos titulos filhos (quando houverem)
        If __lLocBRA .And. !(Alltrim(SE2->E2_TIPO) $ MVTAXA+"|"+MVTXA)
            nRecno  := SE2->(Recno())
            cChave  := SE2->E2_FILIAL+SE2->E2_PREFIXO+SE2->E2_NUM
            cCodRet := SE2->E2_CODRET
            cDirf   := Iif((SE2->E2_DIRF != cDirfImp .AND. cDirfAlt != '1').OR.(SE2->E2_DIRF=="1" .AND.cDirfAlt == "1") , SE2->E2_DIRF, cDirfImp)
            cTxDirf := If(lAltPa,MVTXA,MVTAXA)
            lFoundTx := .F.

            If SE2->E2_DIRF != cDirfImp
                //Se houve alteração do status da DIRF, atualizo os TXs e o tit. principal na sequencia.
                dbSeek(SE2->E2_FILIAL+SE2->E2_PREFIXO+SE2->E2_NUM,.T.)
                Do While !EOF() .And. SE2->E2_FILIAL+SE2->E2_PREFIXO+SE2->E2_NUM == cChave
                    If Alltrim(SE2->E2_TIPO) == cTxDirf .And. Alltrim(SE2->E2_NATUREZ) $ "IRF/PIS/COFINS/CSLL"
                        RecLock("SE2",.F.,,.T.)
                        SE2->E2_DIRF := cDirf
                        lFoundTx := .T.
                        If "IRF" $ SE2->E2_NATUREZ
                            SE2->E2_CODRET := cCodRet
                        Endif
                        If Alltrim(SE2->E2_NATUREZ) $ "PIS/COFINS/CSLL"
                            //uso de código único de retenção - empresa pública
                            If __lFNCDRET
                                aCRets :=ExecBlock("FINCDRET")
                                If aScan(aCRets,cCodRet) > 0
                                    SE2->E2_CODRET := cCodRet
                                EndIf
                            EndIf
                        EndIf
                        MsUnlock()
                    Endif
                    dbSkip()
                Enddo
                SE2->(dbGoto(nRecno))
                RecLock("SE2",.F.,,.T.)
                SE2->E2_DIRF := If(lFoundTx,"2",cDirf)
                MsUnlock()
            Else
                //Se não houve alteração do status da DIRF, restauro o valor original.
                RecLock("SE2",.F.,,.T.)
                SE2->E2_DIRF := cDirfPai
                MsUnlock()
            Endif
        Endif

        If IntePMS()
            SetKey(VK_F10, Nil)
        EndIf
        If SE2->E2_INSS > 0 .and. !lAltPA
            Reclock("SE2",.F.,,.T.)
            SE2->E2_VRETINS := SE2->E2_INSS
            MsUnlock()
        EndIf

        If 	cPaisLoc $ "DOM|COS"  .And. !lF050Auto  .And. (SE2->E2_NATUREZ <> M->E2_NATUREZ .Or. SE2->E2_VALOR <> M->E2_VALOR)
            //Deleção dos titulos de Abatimento Gerados Anteriormente
            fa050DelRet()
            //Geração das Retenções de Impostos - Republica Dominicana //1-Contas a Pagar ou 3-Ambos e Fato Gerador 1-Emissao.
            fa050CalcRet("'1|3'", "2", SE2->E2_NATUREZ, SE2->E2_VALOR, SE2->E2_PREFIXO, SE2->E2_NUM, SE2->E2_FORNECE)
        EndIf

        If nOpcA == 1 .AND. GetNewPar('MV_NGMNTFI','N') == 'S'  .and. !lAltPA
            NGMNTSE2(nOpc)
        Endif

        // Integração protheus X tin.
        If nOpcA == 1 .and. __lHasEAI .and. !lAltPA
            lRatPrj := PMSRatPrj("SE2",,SE2->E2_PREFIXO,SE2->E2_NUM,SE2->E2_PARCELA,SE2->E2_TIPO,SE2->E2_FORNECE,SE2->E2_LOJA)
            If !( AllTrim(SE2->E2_TIPO) $ MVPAGANT .and. lRatPrj  .and. !(cPaisLoc $ "BRA|")) //nao integra PA  para Totvs Obras e Projetos Localizado
                aEaiRet := FWIntegDef('FINA050',,,, 'FINA050')
                If !aEaiRet[1]
                    Help(" ", 1, "HELP", STR0315, STR0316 + CRLF + aEAIRET[2], 3, 1)  // "Erro EAI" / "Problemas na integração EAI. Transação não executada."
                    DisarmTransaction()
                    nOpcA := 2
                    Break
                Endif
            Endif
        Endif

    End Transaction

    If __lLocBRA
        F986LimpaVar() //Limpa as variaveis estaticas - Complemento de Titulo
        f050LRatIR(.T.)
    EndIf

    // Limpa os dados do HashMap
    If __lFlagFKF
        __oHsREINF:Clean() 
        __oHsREINF := Nil
    EndIf

    RestArea(aAreaSE2)
    FwFreeArray(aAreaSE2)

Return nOPCA

//-------------------------------------------------------------------
/*/{Protheus.doc} Fa050Pai

Procura titulo pai do TX/TXA (Imposto) posicionado

@nRecno - Recno do título filho (Imposto)

@author Pilar S. Albaladejo
@since  28/08/95
@version 12
/*/
//-------------------------------------------------------------------
Function Fa050Pai(nRecno)

    Local lAchou    := .F.
    Local cTitPai   := ""
    Local aArea     := GetArea()
    Local aAreaSE2  := SE2->(GetArea())

    Default nRecno := 0

    // Recno do Titulo Pai
    nRecPai := If(Type("nRecPai") == "U", 0, nRecPai)

    // Posiciona no recno do título filho
    If !Empty(nRecno)
        SE2->(DbGoTo(nRecno))
    EndIf

    SE2->(DbSetOrder(1)) // E2_FILIAL, E2_PREFIXO, E2_NUM, E2_PARCELA, E2_TIPO, E2_FORNECE, E2_LOJA
    cTitPai := SE2->E2_TITPAI

    If SE2->( DbSeek(xFilial("SE2") + cTitPai) ) // Busca Título PAI
        lAchou := .T.
        nRecPai := SE2->(Recno())
    EndIf

    RestArea(aAreaSE2)
    RestArea(aArea)

Return lAchou

//-------------------------------------------------------------------
/*/{Protheus.doc} fa050valor

Trata o valor do titulo

@author Wagner Xavier
@since  18/05/93
@version 12
/*/
//-------------------------------------------------------------------
Function fa050valor()

    Local lRet      := .T.
    Local aArea     := GetArea()
    Local aAreaSE2  := {}
    Local nTotAbat  := 0
    Local nRecnoNF  := 0

    If Type("lAltera")=="U"
        If Funname() == "FINA450" .and. Type("lF080Auto") <> "U"
            lAltera := .T.
        Else
            lAltera := .F.
        Endif
    EndIf

    If m->e2_moeda > 99 
        lRet := .F.
    EndIf

    If Type("cTitPaiAB") == "U" .And. lAltera
         cTitPaiAB := ALLTRIM(SE2->E2_TITPAI)
    EndIf 

    //A moeda do abatimento e titulo devem ser as mesmas para compatibilizacao com multi-moedas e taxas variaveis.
    //Isto evita diferencas na consulta FINC060.
    If lRet .and. M->E2_TIPO $ MVABATIM .And. M->E2_MOEDA <> SE2->E2_MOEDA
        Help(" ",1,"E2MOEDIF")
        lRet := .F.
    EndIf

    If lRet .and. lAltera
        // Verifica se o titulo ‚ tipo PA, n†o permitindo altera‡†o do valor
        IF SE2->E2_LA = "S"
            Help(" ",1,"NAOVALOR")
            lRet:=.f.
        ElseIf SE2->E2_TIPO $ MVPAGANT
            Help( " ",1,"FA040ADTO")
            lRet := .F.
        Endif
    Endif

    //Se o titulo tiver desdobramento, devera recalcular a parcela
    If lRet .and. M->E2_DESDOBR == 'S' .And. !IsBlind()
        MsgInfo(STR0344 + CRLF + CRLF + STR0345, STR0356) //"Esse título foi gerado via desdobramento. Os valores das demais parcelas não serão recalculados." ## "Para ajustar o valor das outras parcelas, será necessário editá-las manualmente." ## "Alteração de valor"
    EndIf
       
    nTotAbat := M->E2_VALOR    
   
    // Verifica se o abatimento e' maior que valor do titulo
    IF  lRet .and. !Empty( m->e2_tipo )
        IF m->e2_tipo $ MVABATIM
            aAreaSE2 := SE2->(GetArea())
            SE2->(dbSetOrder(6)) // E2_FILIAL+E2_FORNECE+E2_LOJA+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO
            IF SE2->(dbSeek( xFilial("SE2")+m->e2_fornece+m->e2_loja+m->e2_prefixo + m->e2_num + m->e2_parcela  ))
                While SE2->(!Eof()) .and. SE2->(E2_FORNECE+E2_LOJA+E2_PREFIXO+E2_NUM+E2_PARCELA) == m->e2_fornece+m->e2_loja+m->e2_prefixo+m->e2_num+m->e2_parcela 
                    IF SE2->E2_TIPO $ MVABATIM .And. !(lAltera .And. SE2->(E2_FORNECE+E2_LOJA+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO) == m->e2_fornece+m->e2_loja+m->e2_prefixo+m->e2_num+m->e2_parcela+m->e2_tipo)
                        If AllTrim(SE2->E2_TITPAI) == AllTrim(cTitPaiAB)
                            nTotAbat += SE2->E2_VALOR                         
                        EndIf    
                    Endif

                    If AllTrim(SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)) == AllTrim(cTitPaiAB)
                        nRecnoNF := SE2->(Recno())
                    EndIf
                    
                    SE2->(dbSkip())                   
                Enddo
                If nRecnoNF > 0
                    SE2->(DbGoto(nRecnoNF))                    
                EndIf 

                IF nTotAbat > SE2->E2_SALDO
                    Help(" ",1,"ABATMAIOR")
                    lRet := .f.                
                Endif 
            Endif
            RestArea(aAreaSE2)
        Endif
    Endif

    If lRet
        // Inicializa o valor em Real como sugest†o
        M->E2_VLCRUZ:= Round(xMoeda(M->E2_VALOR,M->E2_MOEDA,1,M->E2_EMISSAO,MsDecimais(1)+1,M->E2_TXMOEDA),MsDecimais(1))
    Endif

    RestArea(aArea)

    FwFreeArray(aArea)
    FwFreeArray(aAreaSE2)

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} fa050Cheque

Teste se o cheque do PA ja' existe.

@author Wagner Xavier
@since  27/04/92
@version 12
/*/
//-------------------------------------------------------------------
Function fa050Cheque(cBanco,cAgencia,cConta,cCheque,lVazio)

    LOCAL cAlias	:= Alias()
    LOCAL lRet		:= .T.
    Local lF050CHEQ := ExistBlock("F050CHEQ")

    lVazio := Iif(lVazio == Nil, .T., lVazio)

    If "FINA050" $ FUNNAME() .and. SubStr(cCheque,1,1) == "*" .And. mv_par05 == 1
        Help(" ",1,"F050CHQINV")
        lRet := .F.
    Else
        If lF050CHEQ
            lRet := ExecBlock("F050CHEQ",.F.,.F.)
        Else
            If Empty(cCheque)
                If	lVazio
                    lRet := .T.
                Else
                    Help( " ", 1, "FA050CHOB",, STR0154, 4, 0 ) // "O numero do cheque é obrigatório "
                    lRet := .F.
                Endif
            Else
                lVazio := .F.
            EndIf

            //Se for obrigatorio o preenchimento do cheque, verifico se o mesmo existe na base
            If lRet .and. !lVazio
                SEF->(dbSetOrder(1))
                If SEF->(dbSeek(xFilial("SEF") + cBanco + cAgencia + cConta + cCheque))
                    Help(" ",,"F050CHEQUE",,STR0357,1,0,,,,,, {STR0358 + CRLF + STR0359 + CRLF + STR0360 + CRLF + STR0361 + CRLF + STR0362 })
                    lRet := .F.
                Endif
            Endif
        EndIf
    Endif

    dbSelectArea(cAlias)

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} Fa050Moed

Verifica se a moeda existe no SX3

@author Pilar S. Albaladejo
@since  10/03/97
@version 12
/*/
//-------------------------------------------------------------------
Function Fa050Moed()

    LOCAL cAlias	:= Alias()
    LOCAL nOrder	:= IndexOrd()
    LOCAL nRec
    LOCAL lRet 		:= .t.
    LOCAL lMoedBco	:= SuperGetMv("MV_MOEDBCO",,.F.)

    //Verifica se a moeda existe no SX3
    cMoeda := Alltrim(Str(m->e2_moeda))
    dbSelectArea("SX3")
    nRec := Recno()
    dbSetOrder(2)
    If !dbSeek("M2_MOEDA"+cMoeda)
        Help ( " ", 1, "SEMMOEDA" )
        lRet := .F.
    EndIf

    If !Empty(cBancoAdt)
        dbSelectArea("SA6")
        dbSetOrder(1)
        dbSeek(xFilial("SA6") + cBancoAdt + cAgenciaAdt + cNumCon)

        If !Empty(SA6->A6_MOEDA) .And. SA6->A6_MOEDA <> 1 .and. M->E2_TIPO == MVPAGANT .and. SA6->A6_MOEDA != M->E2_MOEDA .and. !lMoedBco
            Help ( " ", 1, "MOEDDIF" )
            lRet := .F.
        EndIf
    EndIf

    If cPaisLoc == "RUS"
        M->E2_TXMOEDA := RecMoeda(M->E2_EMISSAO,M->E2_MOEDA)
    EndIf

    dbGoto(nRec)
    dbSelectArea(cAlias)
    dbSetOrder(nOrder)

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} Fa050Inverte

Marca e Desmarca Titulos, invertendo a marca‡†o existente

@author Wagner Xavier
@since  07/11/95
@version 12
/*/
//-------------------------------------------------------------------
Static Function Fa050Inverte(cMarca,oValor,oQtdTit,nValor,nQtdTit,oMark,nMoeda,aChaveLbn,cChaveLbn,lTodos,nRegSel)

    LOCAL nReg 		:= __SUBS->(Recno())
    Local nAscan
    Local lAbreDlgCC := .F.
    Local lF050NPROV := ExistBlock("F050NPROV")

    dbSelectArea("__SUBS")
    If lTodos
        dbSeek(xFilial("SE2"))
    Endif
    While !lTodos .Or. !Eof() .and. xFilial("SE2") == E2_FILIAL

        If lTodos .Or. cChaveLbn == Nil
            cChaveLbn := "SUBS" + xFilial("SE2")+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA
        Endif

        If (lTodos .And. LockByName(cChaveLbn,.T.,.F.)) .Or. !lTodos
            __SUBS->(MsRLock())
            IF E2_OK == cMarca
                __SUBS->E2_OK := "  "
                nValor -= Round(NoRound(xMoeda(E2_SALDO+E2_ACRESC-E2_DECRESC,E2_MOEDA,nMoeda,,3),3),2)
                nQtdTit--
                nAscan := Ascan(aChaveLbn, cChaveLbn )
                If nAscan > 0
                    UnLockByName(aChaveLbn[nAscan],.T.,.F.) // Libera Lock
                Endif
            Else
                If Ascan(aChaveLbn, cChaveLbn) == 0
                    Aadd(aChaveLbn,cChaveLbn)
                Endif
                __SUBS->E2_OK := cMarca
                nValor += Round(NoRound(xMoeda(E2_SALDO+E2_ACRESC-E2_DECRESC,E2_MOEDA,nMoeda,,3),3),2)
                nQtdTit++
                nRegSel := __SUBS->NUM_REG
            Endif
            __SUBS->(MsUnlock())

            If lF050NPROV
                ExecBlock("F050NPROV",.F.,.F.,{cChaveLbn})
            EndIf
            If cPaisLoc == "EQU"
                lAbreDlgCC := .F.
                If SE2->E2_TIPO <> "CC "
                    SF1->(dbSetOrder(1))
                    If SF1->(dbSeek(xFilial("SF1")+SE2->E2_NUM+SE2->E2_PREFIXO+SE2->E2_FORNECE+SE2->E2_LOJA))
                        SE4->(dbSetOrder(1))
                        If SE4->(dbSeek(xFilial("SE4")+SF1->F1_COND)) .and. AllTrim(SE4->E4_FORMA) == "CC"
                            lAbreDlgCC := .T.
                        EndIf
                    EndIf
                Else
                    lAbreDlgCC := .T.
                EndIf
                If !Empty(__SUBS->E2_OK) .and. lAbreDlgCC
                    //Executar dialogo para obter os dados do Cartão de Crédito
                    Fa050GetCC(.F.)
                EndIf
            EndIf
        Endif
        If !lTodos
            Exit
        Endif
        dbSkip()
    Enddo

    __SUBS->(dbGoto(nReg))
    oValor:Refresh()
    oQtdTit:Refresh()
    oMark:oBrowse:Refresh(.t.)

Return Nil

//-------------------------------------------------------
/*/{Protheus.doc} F050Dsdobr
	Faz desdobramento em parcelas, do titulo em inclusao.

@author Claudio D. de Souza
@since 31/08/1998
@version P12
/*/
//-------------------------------------------------------
Function F050Dsdobr()

    Local nOpcDsd:= 0
    Local cCondPgto:= Space(3), nParceDsd:= 0, cValorDsd := "T"
    Local nPerioDsd:= 0
    Local nOrdSE2 := SE2->(IndexOrd())
    Local oDlg, oVlDsd
    Local lCondPgto := .F.
    Local lFina250		:= FwIsInCallStack("F050DELRTD")
    Local lF05MONTDD := ExistBlock ("F05MONTDD")

    //Verifico se permite rateios de desdobramento no mesmo titulo
    If __lRatDsd == NIL
        __lRatDsd := IIF(GetMv("MV_RATDESD",,"2") == "1", .T., .F.)
    Endif

    aParcelas := {}
    aParcacre := {}
    aParcdecre:= {}
    //caso seja executado esse PE, ele deve retornar:
    // .T. para informar que já fez o tratamento necessário e a tela de desdobramento padrao NAO DEVE ser apresentada
    // .F. para informar que nao fez o tratamento necessário e a tela de desdobramento padrao DEVE ser apresentada
    If lF05MONTDD .and. ExecBlock("F05MONTDD",.F.,.F.)
        M->E2_MULTNAT := "2"
        Return .T.
    EndIF

    //Exclusao chamada a partir do cancelamento de desdobramento com rastreamento e calculo de impostos
    If lFina250
        M->E2_DESDOBR := "N"
        Return .T.
    Endif
    //Verifica se a campos obrigatorios foram preencidos
    If Empty(m->e2_num) 		.or. Empty(m->e2_tipo)    .or. Empty(m->e2_naturez) .or.;
        Empty(m->e2_fornece)	.or. Empty(m->e2_loja)    .or. Empty(m->e2_emissao) .or.;
        Empty(m->e2_vencto) 	.or. Empty(m->e2_vencrea) .or. Empty(m->e2_valor)   .or.;
        Empty(m->e2_vlcruz)
        If !lF050Auto
            Help(" " , 1 , "FA050NODSD")
        Endif
        Return .F.
    Endif
    If m->e2_tipo $ MVPAGANT+"/"+MV_CPNEG+"/"+MVABATIM
        If !lF050Auto
            Help(" " , 1 , "FA050TPDSD")
        Endif
        Return .F.
    Endif

    If m->E2_RATEIO == "S" .And. !__lRatDsd// Se nao rateia desdobramento
        If !lF050Auto
            Help(" " , 1 , "FA050NORAT")
        Endif
        Return .F.
    Endif

    If !lF050Auto

        nOpcDsd := 0
        While	nOpcDsd == 0

            DEFINE MSDIALOG oDlg FROM	0,0 TO 235,290 TITLE STR0057 PIXEL  //"Desdobramento"
            @ 004, 007 TO 105, 105 OF oDlg PIXEL
            @ 010, 014 SAY STR0058 SIZE 90, 7 OF oDlg PIXEL  //"Condicao de Pagamento"
            @ 028, 014 SAY STR0059 SIZE 90, 7 OF oDlg PIXEL  //"Numero de Parcelas"
            @ 046, 014 SAY STR0060 SIZE 90, 7 OF oDlg PIXEL  //"Valor do Titulo (Total ou Parcela)"
            @ 064, 014 SAY STR0061 SIZE 90, 7 OF oDlg PIXEL  //"Periodo de Vencto. (em dias)"
            @ 082, 014 SAY STR0062 SIZE 90, 7 OF oDlg PIXEL  //"Historico"
            @ 018, 014 MSGET cCondPgto	F3 "SE4" Picture "!!!" SIZE 72, 08 OF oDlg PIXEL ;
                                                    VALID (Empty (cCondPgto) .or. ExistCpo("SE4",cCondPgto)) .and. Fa290Cond(cCondPgto) HASBUTTON
            @ 036, 014 MSGET  nParceDsd	Picture "99999" When IIf(Empty(cCondPgto),.T.,.F.);
                                                    VALID f050ValPar(nParceDsd,nMaxParc) ;
                                                    SIZE 80, 08 OF oDlg PIXEL
            @ 054, 014 MSCOMBOBOX oVlDsd VAR cValorDsd ITEMS {STR0069,STR0064} SIZE 80, 10 OF oDlg PIXEL ; //"TOTAL"###"PARCELA"
                                                    When IIf(Empty(cCondPgto),.T.,.F.)
            @ 072, 014 MSGET nPerioDsd				Picture "999" When IIf(Empty(cCondPgto),.T.,.F.) ;
                                                    VALID nPerioDsd > 0;
                                                    SIZE 80, 08 OF oDlg PIXEL
            @ 090, 014 MSGET  cHistDsd			 	Picture "@S40";
                                                    SIZE 80, 08 OF oDlg PIXEL

            DEFINE SBUTTON FROM 07, 110 TYPE 1 ACTION ;
            {||nOpcDsd:=1,IF(A050TudoOK(cCondPgto,nParceDsd,cValorDsd,nPerioDsd),oDlg:End(),nOpcDsd:=0)} ENABLE OF oDlg
            DEFINE SBUTTON FROM 23, 110 TYPE 2 ACTION {||nOpcDsd:=9 ,oDlg:End()} ENABLE OF oDlg

            ACTIVATE MSDIALOG oDlg CENTERED
        EndDo

    Else  //Rotina Automatica
        nOpcDsd := 1
        aValidGet:= {}
        //Condicao de Pagamento
        IF (nT := ascan(aAutoCab,{|x| x[1]='AUTCDPGDSD'})) > 0
            Aadd(aValidGet,{'cCondPgto' ,aAutoCab[nT,2],'Empty (cCondPgto) .or. (ExistCpo("SE4",cCondPgto)) .and. SE4->E4_TIPO != "9"',.t.})
            lCondPgto := .T.
            cCondPgto := aAutoCab[nT,2]
        Endif

        //Historico
        IF (nT := ascan(aAutoCab,{|x| x[1]='AUTHISTDSD'})) > 0
            cHistDsd := aAutoCab[nT,2]
            Aadd(aValidGet,{'cHistDsd' ,aAutoCab[nT,2],'.T.',.t.})
        Endif

        If !lCondPgto

            //Numero de parcelas
            IF (nT := ascan(aAutoCab,{|x| x[1]='AUTNPARDSD'})) > 0
                Aadd(aValidGet,{'nParceDsd' ,aAutoCab[nT,2],'f050ValPar(nParceDsd,nMaxParc,.T.)',.t.})
                nParceDsd := aAutoCab[nT,2]
            Endif

            //Total ou Parcela
            IF (nT := ascan(aAutoCab,{|x| x[1]='AUTTOPADSD'})) > 0
                Aadd(aValidGet,{'cValorDsd' ,aAutoCab[nT,2],'cValorDsd $ "T#P"',.t.})
                cValorDsd := aAutoCab[nT,2]
            Endif

            //Periodo entre parcelas
            IF (nT := ascan(aAutoCab,{|x| x[1]='AUTPERIDSD'})) > 0
                Aadd(aValidGet,{'nPerioDsd' ,aAutoCab[nT,2],'nPerioDsd > 0',.t.})
                nPerioDsd := aAutoCab[nT,2]
            Endif
        Endif

        If ! SE2->(MsVldGAuto(aValidGet)) // consiste os gets
            nOpcDsd := 2
        EndIf

    Endif

    cSE2TpDsd := cValorDsd

    If nOpcDsd == 1
        nSavRec:=RecNo()
        dbSelectArea("SE2")
        Fa050Cond(cCondPgto,nParceDsd,cValorDsd,nPerioDsd)
        //Cancela Multiplas Naturezas se tiver Desdobramento
        If !__lRatDsd
            M->E2_MULTNAT := "2"
        EndIF
    Else
        Return .F.
    Endif
    dbSelectArea("SE2")
    dbSetOrder(nOrdSE2)

    Return .T.


//-------------------------------------------------------
/*/{Protheus.doc} F050VALPAR

Faz verificacao do numero de parcelas do desdobramento.

@author Mauricio Pequim Jr.
@since 01/09/98
@version P12
/*/
//-------------------------------------------------------
Function F050VALPAR(nParceDsd,nMaxParc,lAuto)

    DEFAULT nParceDsd := 0
    DEFAULT nMaxParc := 0
    DEFAULT lAuto := .F.

    If nParceDsd > nMaxParc .or. nParceDsd < 2
        If !lAuto
            If cPaisLoc == 'ARG'
                MSGINFO(STR0274 + Alltrim(Str(nMaxParc)) + STR0275,  STR0268) // "El Número de Cuotas ha excedido el valor de "//" configurado en el parámetro MV_LIMCUOT "//"Régimen Especial Facilidades Pago"
            Else
                Help(" " , 1 , "FA050PCDSD")
            EndIf
        Endif
        Return .F.
    Endif

Return .T.

//-------------------------------------------------------
/*/{Protheus.doc} Fa050Cond

Faz calculos do Desdobramento parcelas automaticas

@author Mauricio Pequim Jr.
@since 01/09/98
@version P12
/*/
//-------------------------------------------------------
Function Fa050Cond(cCondDsd,nParceDsd,cValorDsd,nPerioDsd)

    Local nValParc		:= 0		// Valor de cada parcela
    Local nValParcAc	:= 0
    Local nValParcDe	:= 0
    Local nVlTotParc	:= 0  	// Valor do somatorio das parcelas
    Local nVlTotAcre	:= 0
    Local nVlTotDecr	:= 0
    Local nDifer		:= 0
    Local nDifacre		:= 0
    Local nDifdecre     := 0
    Local nCond			:= 0
    Local dDtVenc		:= IIF(Empty(cCondDsd),dDataBase,m->e2_emissao)
    Local nValorDSD     := 0
    Local lCalcIssBx    := IsIssBx("P")
    Local lPerPc1		:= .T.
    Local nCondd		:= 0
    Local nConda		:= 0
    Local lPccBaixa     := SuperGetMv("MV_BX10925",.T.,"2") == "1"
    Local lIRPFBaixa    := .F.
    Local lF50DTDSD     := ExistBlock("F50DTDSD")
    Local lF050PRPC     := ExistBlock("F050PRPC")

    // Verifica se é IRPF pela Baixa para guardar os títulos que devem ter retenção
    lIRPFBaixa :=	IIf( __lLocBRA, ;
                    Posicione("SA2",1,xfilial("SA2") + m->(E2_FORNECE+E2_LOJA),"A2_CALCIRF") == "2", .F. ) .And. ;
                    IIf(cPaisLoc $ "ANG|ARG|AUS|BOL|BRA|CHI|COL|COS|DOM|EQU|EUA|HAI|MEX|PAD|PAN|PAR|PER|POR|PTG|SAL|URU|VEN", ;
                    Posicione("SED",1,xfilial("SED") + m->E2_NATUREZ ,"ED_CALCIRF") = "S", .F.)

    If __nVlrMR == 0
        nValorDSD	:= m->e2_valor
        nValorDSD	+= If(!lIRPFBaixa,m->e2_irrf,0)
        nValorDSD	+= If(!lCalcIssBx,m->e2_iss,0)
        nValorDSD	+= m->e2_inss
        nValorDSD	+= If(__lLocBRA,m->e2_sest,0)
        nValorDSD	+= If(!lPccBaixa, m->e2_cofins + m->e2_csll + m->e2_pis, 0)
    Else
        nValorDSD	:= __nVlrMR
    EndIf

    IF SED->ED_DEDINSS == "2"  //Nao desconta o INSS do principal
        nValorDsd -= m->e2_inss
    Endif

    //Zera valor dos impostos para evitar problemas no desdobramento
    m->e2_valor		:= nValorDSD
    m->e2_vlcruz	:= Round( NoRound( xMoeda(m->e2_valor,m->e2_moeda,1,m->e2_emissao,MsDecimais(1)+1,m->e2_txmoeda),MsDecimais(1)+1),MsDecimais(1))
    m->e2_irrf		:= 0
    m->e2_iss		:= 0
    m->e2_inss		:= 0
    m->e2_sest		:= 0
    m->e2_cofins	:= 0
    m->e2_csll		:= 0
    m->e2_pis		:= 0
    m->e2_vretirf	:= 0
    m->e2_vretpis	:= 0
    m->e2_vretcof	:= 0
    m->e2_vretcsl	:= 0
    m->e2_vretins	:= 0
    m->e2_pretins	:= " "

    // Ponto de Entrada F50DTDSD
    // Utilizado para manipulacao de data inicial para os calculos
    // de vencimento das parcelas do desdobramento.
    IF lF50DTDSD
        dDtVenc := ExecBlock("F50DTDSD",.F.,.F.)
    Endif

    // Ponto de Entrada F050PRPC
    // Utilizado para manipulacao da aplica‡Æo ou nao do periodo
    // interparcela sobre a a primeira parcela, dever  retornar
    // retornar .T.(aplica)  ou .F. (nÆo aplica). Exemplo:
    // Tendo como data inicial para calculo 10/02/2002, periodo
    // interparcela de 10 dias, e retorno .T., a data de vencto
    // inicial ser  20/02/2002. Caso retorno seja .F., a data
    // de vencto da primeira parcela ser  10/02/2002. Aplic vel
    // apenas quando NÇO se utilizar condicao de pagamento para
    // calculo dos titulos a serem gerados.
    IF lF050PRPC .and. Empty(cCondDsd)
        lPerPc1 := ExecBlock("F050PRPC",.F.,.F.)
    Endif

    // Caso a data retornada pelo PE acima seja menor que a data
    // de emissao do titulo gerador do desdobramento, utilizo o
    // padrao de inicializacao da data inicial para calculo do
    // vencimento das parcelas.
    If dDtVenc < m->e2_emissao
        If !Empty(cCondDsd)
            dDtVenc := m->e2_emissao
        Else
            dDtVenc := dDataBase
        Endif
    Endif

    If !Empty(cCondDsd)
        aParcelas := Condicao (nValorDsd	,cCondDsd,,dDtVenc)
        aParcacre := Condicao (m->e2_acresc ,cCondDsd,,dDtVenc)
        aParcdecre:= Condicao (m->e2_decresc,cCondDsd,,dDtVenc)
        // Corrige possiveis diferencas entre o valor total e o
        // apurado ap¢s a divisao das parcelas
        For nCond := 1 to Len (aParcelas)
            nVlTotParc += aParcelas [ nCond, 2]
        Next
        If nVlTotParc != nValorDsd
            nDifer := round(nValorDsd - nVlTotParc,2)
            aParcelas [ Len(aParcelas), 2 ] += nDifer
        Endif
        If Len(aParcacre)>0
            For nConda := 1 to Len (aParcacre)
                nVlTotAcre += aParcacre [ nConda, 2]
            Next
            If nVlTotAcre != m->e2_acresc
                nDifacre := round(m->e2_acresc - nVlTotAcre,2)
                aParcelas [ Len(aParcelas), 2 ] += nDifacre
            Endif
        Endif
        If Len(aParcdecre)>0
            For nCondd := 1 to Len (aParcdecre)
                nVlTotDecr += aParcdecre [ nCondd, 2]
            Next
            If nVlTotAcre != m->e2_decresc
                nDifdecre := round(m->e2_decresc - nVlTotDecr,2)
                aParcdecre [ Len(aParcdecre), 2 ] += nDifdecre
            Endif
        Endif
    Else
        // Verifica se o valor do titulo que est  sendo desdobrado ‚ o
        // total, e por consequencia, divide por numero de parcelas ou
        // caso seja o valor da parcela, gera n parcelas do valor.
        If Left(cValorDsd,1) == "T"
            nValParc 	:= Round(NoRound((nValorDsd / nParceDsd),3),2)
            nValParcAc	:= Round(NoRound((m->e2_acresc / nParceDsd),3),2)
            nValParcDe	:= Round(NoRound((m->e2_decresc / nParceDsd),3),2)
        Else
            nValParc	:= nValorDsd
            nValParcAc	:= m->e2_acresc
            nValParcDe	:= m->e2_decresc
        Endif
        For nCond := 1 To nParceDsd
            If (nCond == 1 .and. lPerPc1) .or. nCond > 1
                dDtVenc += nPerioDsd
            Endif
            dDtVencRea := DataValida(dDtVenc,.T.)
            AADD ( aParcelas, { dDtVenc , nValParc } )
            AADD ( aParcacre, { dDtVenc , nValParcAc } )
            AADD ( aParcdecre, { dDtVenc , nValParcDe } )
            nVlTotParc += aParcelas [nCond,2]
            nVlTotAcre += aParcacre [nCond,2]
            nVlTotDecr += aParcdecre [nCond,2]
        Next
        If Left(cValorDsd,1) == "T"
            nDifer		:= Round(nValorDsd - nVlTotParc,2)
            nDifacre	:= Round(m->e2_acresc - nVlTotAcre,2)
            nDifdecre	:= Round(m->e2_decresc - nVlTotDecr,2)
            aParcelas [ Len(aParcelas), 2 ] += nDifer
            aParcacre [ Len(aParcacre), 2 ] += nDifacre
            aParcdecre [ Len(aParcdecre), 2 ] += nDifdecre
        Endif
    Endif

Return .T.

//-------------------------------------------------------
/*/{Protheus.doc} A050TudoOk

Verifica se dados para desdobramento estao corretos.

@author Mauricio Pequim Jr.
@since 08/09/98
@version P12
/*/
//-------------------------------------------------------
Static Function A050TudoOk(cCondPgto,nParceDsd,cValorDsd,nPerioDsd)

    Local lOk := .T.
    Local nTotalCto := 0
    Local nMinRG3806 := SuperGetMV("MV_RG3806",.T.,0)
    Local aTotParc := {}
    Local dDtVenc		:= IIF(Empty(cCondPgto),dDataBase, M->E2_EMISSAO)

    If Empty (cCondPgto)
        If nParceDsd < 2 .or. nParceDsd > nMaxParc .or.	Empty(cValorDsd).or.	nPerioDsd <= 0
            Help(" " , 1 , "FA050DADOS")
            lOk := .F.
        Endif
    Else
        If dDtVenc < M->E2_EMISSAO
            dDtVenc := M->E2_EMISSAO
        Endif

        aTotParc := Condicao (M->E2_VALOR, cCondPgto, ,dDtVenc)

        If Len(aTotParc) > (nMaxParc + 1)
            Help(" " , 1 , "FA050DADOS")
            lOk := .F.
        EndIf
    Endif



    If Empty (cCondPgto) .And. cPaisLoc == "ARG"
        If Left(cValorDsd,1) == "T"
            nTotalCto 	:= Round(NoRound((M->E2_VLCRUZ / nParceDsd),3),2)
        Else
            nTotalCto	:= M->E2_VLCRUZ
        EndIf
        If nTotalCto < nMinRG3806
            MSGINFO(Replace(STR0267, "#cValMin#", Alltrim(Str(nMinRG3806))), STR0268)
            lOK := .F.
        EndIf
    EndIf

Return lOk

//-------------------------------------------------------
/*/{Protheus.doc} F050Ajuda

Help para campos do desdobramento.

@author Mauricio Pequim Jr.
@since 08/09/98
@version P12
/*/
//-------------------------------------------------------
Function F050Ajuda (nOpcHlp)

    If nOpcHlp == 1    		// Condicao de Pagamento
        Help(" ",1,"CCONDPGTO")
    Elseif nOpcHlp == 2   	// Numero de Parcelas
        Help(" ",1,"NPARCELAS")
    Elseif nOpcHlp == 3   	// Tipo do Valor (Total / Parcela)
        Help(" ",1,"CVALORDSD")
    Elseif nOpcHlp == 4   	// Periodo de vencto (em dias)
        Help(" ",1,"NPERIODSD")
    ElseIf nOpcHlp == 5   		// Historico do desdobramento
        Help(" ",1,"CHISTDSD")
    Endif

Return .T.


//-------------------------------------------------------
/*/{Protheus.doc}FA050Nat2
Calcula os impostos quando se muda o valor

@author Wagner Xavier
@since  28/04/92
/*/
//-------------------------------------------------------
Function FA050NAT2(lVlOnlyRet As Logical, cField As Character)

    Local nx		    As Numeric
    Local nValInss      As Numeric
    Local nValSEST      As Numeric
    Local nValFrete     As Numeric
    Local nValIRRF      As Numeric
    Local nPercIss      As Numeric
    Local nLimInss      As Numeric
    Local nINSSRet      As Numeric //--Valor do INSS retido no periodo
    Local aAreaSE2      As Array
    Local aAreaSED      As Array
    Local lAplicaTP     As Logical
    Local lRndVlIss     As Logical
    Local nBaseIrrf     As Numeric
    Local nBaseDep      As Numeric
    Local nBaseCide     As Numeric
    Local nValDep       As Numeric
    Local nBasePCC      As Numeric
    Local lOk           As Logical
    Local lF050FCTC     As Logical
    Local lPCCBaixa     As Logical //Controla o Pis Cofins e Csll na baixa

    //IRPF na baixa
    Local lIRPFBaixa    As Logical
    Local lBaseIRPF	    As Logical
    // Carrega variavel de verificacao de consideracao de valor minimo de retencao de IR.
    Local lAplMinIR     As Logical
    Local lCalcIssBx    As Logical
    Local lSimples      As Logical //-- Optante pelo simples
    Local lEmprInd      As Logical //-- Empresa Individual
    Local lVerMinIss 	As Logical
    Local lPaBruto      As Logical  //Indica se o PA terá o valor dos impostos descontados do seu valor
    Local cTipUso       As Character
    Local nIrfInss      As Numeric
    Local cQuery        As Character
	Local cAliasQry     As Character
	Local lCIDE  	    As Logical // Define o fato gerador do imposto CIDE. 1 = Baixa ou 2 = Emissão
    Local nCalcInss 	As Numeric
    Local nINSSTot 	    As Numeric
    Local cTipCTC       As Character  // Tipo Contrato de Carreteiro
    Local cVenctoPF     As Character  //1 = Emissao    2= Vencimento Real	3=Data Contabilizacao
    Local cAglImPJ	    As Character
    Local nLoop         As Numeric
    Local aFilial	    As Array
    Local aCliFor	    As Array
    Local cArqTmp	    As Character
    Local lDelTrbIR     As Logical
    Local lBaseDif	    As Logical
    Local lFINA050      As Logical
    Local lDedIns	    As Logical
    Local nBaseIss	    As Numeric
    Local nBaseIns	    As Numeric    
    Local lDescIr       As Logical 
    //Base de imposto Variavel
    Local lBaseImp	    As Logical //Verifica a existência dos campos e o calculo de impostos
    Local lCpoValor	    As Logical
    Local lF050GRVL	    As Logical // ponto de entrada para resgatar o valor E2_VALOR antes que o sistema efetue qualquer calculo de imposto (EIC)
    Local lValFre       As Logical
    Local lEasyFin	    As Logical
    Local lRefImp	    As Logical //-- Usado pelo TMS com Operadora de Frota
    Local lTmsOper	    As Logical
    Local lAltVcto	    As Logical // Altera valor somente em determinado momento
    //Variavel indica se irá provisionar os impostos de INSS e ISS na inclusão da PA, deduzindo-os do valor de adiantamento.
    Local lPrImPA       As Logical
    //--- Tratamento Gestao Corporativa
    Local cLayout       As Character
    Local lGestao	    As Logical
    Local cFilFwSE2     As Character
    Local cFilFwSA2     As Character
    Local nVlMinImp     As Numeric
    Local nCalcIr	    As Numeric
    Local lAcmPJ 	    As Logical //1 = Acumula    2= Não acumula
    Local nMinINS1      As Numeric
    Local nMinINS2      As Numeric
    //Inss Baixa com empresa publica. Neste caso os valores do inss não tem valor minimo ou maximo de retencao.
    Local lInsPub       As Logical
    Local lJaDescIr     As Logical
    Local aPCC		    As Array
    Local lEmpPub       As Logical
    Local nVencto 	    As Numeric
    Local nVlMPub	    As Numeric
    Local dRef		    As Date
    Local lSumIR        As Logical
    Local nValIrOld     As Numeric
    Local cChaveSA2     As Character
    Local aAreaSA2	    As Array
    Local lAplMinP      As Logical
    Local lF050INBR     As Logical
    Local nBasM2Irf     As Numeric
    Local lRndSest      As Logical
    Local lDsdobra	    As Logical
    Local aISSCPOM      As Array // 1pos - UF, 2pos - Municipio, 3pos - Aliquota, 4pos - Vlr.Min
    Local cFunname	    As Character
    Local lParcela      As Logical
    Local lF050ATP		As Logical
    Local lF050CIRF 	As Logical
    // Verifica se a retencao minima de ISS por base de calculo devera ser calculada pela emissao e nao vencimento - Municipio de Itabira - MG
    Local lRetISSEmi	As Logical
    Local aCalcIss      As Array
    Local nCalcIss      As Numeric
    Local nRetIss	    As Numeric
    Local nBasRIss      As Numeric
    Local nVRetISS	    As Numeric
    Local nTotBasISS    As Numeric
    Local lCPRecISS		As Logical
    Local lTitRetISS	As Logical
    Local aRecSE2		As Array
    Local nValPIS		As Numeric
    Local nValCOF		As Numeric
    Local nValCSL		As Numeric
  
    Local lIsLotePLS	As Logical
    Local lIrfRetAnt	As Logical

    Local dVigMP1171    As Date
    Local nBasOrig      As Numeric
    Local nVrIrDedS     As Numeric   
    Local lIrTabSimp    As Logical

    //Variaveis declaradas para uso da F080TotMes(), que varre o SE5 buscando os titulos que estao pendente recolhimento (pis, cofins e csll)
    Private nPis        As Numeric
    Private nCofins     As Numeric
    Private nCsll       As Numeric
    Private nValPgto    As Numeric
    Private nIrrf       As Numeric
    Private nOldValPgto As Numeric
    //Republica Dominicana
    Private nImpost01	As Numeric
    Private nImpost02   As Numeric
    Private aImpostos  	As Array
    Private nPos        As Numeric

    Default lVlOnlyRet 	:= .F.
    Default cField	    := Readvar()
    Default __lTemMR    := (FindFunction("FTemMotor") .and. FTemMotor())
    Default __lFnBtr    := FindFunction("ISSCPOM") .And. FindFunction("BtrISSMun")
    Default __lBtrISS   := SE2->(ColumnPos("E2_BTRISS")) > 0 .And. SE2->(ColumnPos("E2_VRETBIS")) > 0 .And. SE2->(ColumnPos("E2_CODSERV")) > 0 .And. __lFnBtr

    nx		      := 0
    nValInss      := 0
    nValSEST      := 0
    nValFrete     := 0
    nValIRRF      := 0
    nPercIss      := 0
    nLimInss      := GetMv("MV_LIMINSS",.F.,0)
    nINSSRet      := 0 //--Valor do INSS retido no periodo
    aAreaSE2      := {}
    aAreaSED      := {}
    lAplicaTP     := .T.
    lRndVlIss     := SuperGetMv("MV_RNDISS",.F.,.F.)
    nBaseIrrf     := 0
    nBaseDep      := GetMV("MV_TMSVDEP",,0)
    nBaseCide     := 0
    nValDep       := 0
    nBasePCC      := 0
    lOk           := .T.
    lF050FCTC     := ExistBlock('F050FCTC')
    lPCCBaixa     := SuperGetMv("MV_BX10925",.T.,"2") == "1" //Controla o Pis Cofins e Csll na baixa

    //IRPF na baixa
    lIRPFBaixa    := IIf( __lLocBRA, SA2->A2_CALCIRF == "2", .F.)
    lBaseIRPF	    := F050BIRPF()
    // Carrega variavel de verificacao de consideracao de valor minimo de retencao de IR.
    lAplMinIR     := .F.
    lCalcIssBx    := IsIssBx("P")
    lSimples      := __lLocBRA .and. SA2->A2_CALCIRF == "3" //-- Optante pelo simples
    lEmprInd      := __lLocBRA .and. SA2->A2_CALCIRF == "4" //-- Empresa Individual
    lVerMinIss    := .T.
    lPaBruto      := GetNewPar("MV_PABRUTO","2") == "1"  //Indica se o PA terá o valor dos impostos descontados do seu valor
    cTipUso       := IIf(nModulo==43,"1","2")
    nIrfInss      := 0
    cQuery        := ""
	cAliasQry     := ""
	lCIDE  	      := cPaisLoc == "BRA" .And. SuperGetMv("MV_FGCIDE",.T.,"2") == "2" // Define o fato gerador do imposto CIDE. 1 = Baixa ou 2 = Emissão
    nCalcInss 	  := 0
    nINSSTot 	  := 0
    cTipCTC       := Padr( SuperGetMv("MV_TPTCTC",.T.,""), Len( SE2->E2_TIPO ) ) // Tipo Contrato de Carreteiro
    cVenctoPF     := SuperGetMv("MV_ACMIRPF",.T.,"2")  //1 = Emissao    2= Vencimento Real	3=Data Contabilizacao
    cAglImPJ	  := SuperGetMv("MV_AGLIMPJ",.T.,"1")
    nLoop         := 0
    aFilial	      := {}
    aCliFor	      := {}
    cArqTmp	      := ""
    lDelTrbIR     := .T.
    lBaseDif	  := cPaisLoc $ "ANG|ARG|AUS|BOL|BRA|CHI|COL|COS|DOM|EQU|EUA|HAI|PAD|PAN|PAR|PER|POR|PTG|SAL|TRI|URU|VEN"
    lFINA050      := FunName() $ "FINA050" .or. (FwIsInCallStacK("Fin750050"))
    lDedIns	      := (SuperGetMv("MV_INSIRF",.F.,"2") == "1")
    nBaseIss	  := 0
    nBaseIns	  := 0    
    lDescIr       := .F. 
    //Base de imposto Variavel
    lBaseImp	  := F050BSIMP(2) //Verifica a existência dos campos e o calculo de impostos
    lCpoValor	  := .F.
    lF050GRVL	  := ExistBlock("F050GRVL") // ponto de entrada para resgatar o valor E2_VALOR antes que o sistema efetue qualquer calculo de imposto (EIC)
    lValFre       := .F.
    lEasyFin	  := GetNewPar("MV_EASYFIN","N")=="S"
    lRefImp	      := SuperGetMv('MV_REFIMP',,.F.)  //-- Usado pelo TMS com Operadora de Frota
    lTmsOper	  := SuperGetMv('MV_VSREPOM',,'1')  == '2' .And. SuperGetMv('MV_TMSOPDG',,'1')  == '2'
    lAltVcto	  := "E2_VENCREA" $ Upper(AllTrim(ReadVar())) .and. M->E2_VALOR == 0 // Altera valor somente em determinado momento
    //Variavel indica se irá provisionar os impostos de INSS e ISS na inclusão da PA, deduzindo-os do valor de adiantamento.
    lPrImPA       := !lPaBruto .And. (SuperGetMv("MV_PAPRIME",.T.,"2") == "1")
    //--- Tratamento Gestao Corporativa
    cLayout       := FWSM0Layout()
    lGestao	      := "E" $ cLayout .Or. "U" $ cLayout
    cFilFwSE2     := IIF( lGestao , FwFilial("SE2") , xFilial("SE2") )
    cFilFwSA2     := IIF( lGestao , FwFilial("SA2") , xFilial("SA2") )
    nVlMinImp     := GetNewPar("MV_VL10925",5000)
    nCalcIr	      := 0
    lAcmPJ 	      := SuperGetMv("MV_INSACPJ",.T.,"2") == "1" //1 = Acumula    2= Não acumula
    nMinINS1      := SuperGetMv("MV_MININSS",.F.,0)
    nMinINS2      := SuperGetMv("MV_VLRETIN",.F.,0)
    //Inss Baixa com empresa publica. Neste caso os valores do inss não tem valor minimo ou maximo de retencao.
    lInsPub       := SuperGetMv("MV_INSPUB",,.F.) .And. nMinINS1 == 0 .And. nLimInss == 0 .And. nMinINS2 == 0
    lJaDescIr     := .F.
    aPCC		  := Array(4)
    lEmpPub       := IsEmpPub()
    nVencto 	  := SuperGetMv("MV_VCPCCP",.T.,1)
    nVlMPub	      := SuperGetMv("MV_VLMPUB",.T.,10)
    dRef		  := dDatabase
    lSumIR        := .F.
    nValIrOld     := 0
    cChaveSA2     := ""
    aAreaSA2	  := {}
    lAplMinP      := .F.
    lF050INBR     := ExistBlock("F050INBR")
    nBasM2Irf     := 0
    lRndSest      := SuperGetMv("MV_RNDSEST",.F.,.F.)
    lDsdobra	  := If(FindFunction("PrinDesdobr"), PrinDesdobr(), M->E2_DESDOBR == "S" )
    aISSCPOM      := {} // 1pos - UF, 2pos - Municipio, 3pos - Aliquota, 4pos - Vlr.Min
    cFunname	  := FUNNAME()
    lParcela      := .F.
    lF050ATP	  := ExistBlock("F050ATP")
    lF050CIRF 	  := ExistBlock("F050CIRF")
    // Verifica se a retencao minima de ISS por base de calculo devera ser calculada pela emissao e nao vencimento - Municipio de Itabira - MG
    lRetISSEmi	  := GetNewPar("MV_RISSEMI",.F.)
    aCalcIss      := Array(0)
    nCalcIss      := 0
    nRetIss	      := 0
    nBasRIss      := 0
    nVRetISS	  := 0
    nTotBasISS    := 0
    lCPRecISS	  := .F.
    lTitRetISS	  := .F.
    aRecSE2		  := {}
    nValPIS		  := 0
    nValCOF		  := 0
    nValCSL		  := 0

    lIsLotePLS	  := IsInCallStack("PLSA470")
    lIrfRetAnt    := .F.

    nBasOrig   := 0
    nVrIrDedS  := 0
    lIrTabSimp := SuperGetMV("MV_FMP1171",.F.,.F.) //Habilita calculo do IRPF c/ dedução simplificada
    dVigMP1171 := CTOD("01/05/2023") //Inicio da vigencia da MP 1.171/23

    //Variaveis declaradas para uso da F080TotMes(), que varre o SE5 buscando os titulos que estao pendente recolhimento (pis, cofins e csll)
    nPis          := 0
    nCofins       := 0
    nCsll         := 0
    nValPgto      := 0
    nIrrf         := 0
    nOldValPgto   := 0
    //Republica Dominicana
    nImpost01	  := 0
    nImpost02     := 0
    aImpostos  	  := {}
    nPos          := 0

    aPCC[1]       := .F.
    __aTitCalc	  := {}

    If cPaisLoc == "BRA"
        __lDedSimpl := .F.
    Endif

    If lRefImp .And. lTmsOper .And. (FwIsInCallStack('TMSQUITAC') .Or. FwIsInCallStack('TMA250SE2'))
        lVlOnlyRet := .T.
    EndIf

    IF lCpoValor .AND. lF050GRVL
        ExecBlock("F050GRVL",.F.,.F.)
    Endif

    If Type("lAltera") == "U"
        If Funname() == "FINA450" .and. Type("lF080Auto") <> "U"
            lAltera := .T.
        Else
            lAltera := .F.
        Endif
    EndIf

    If Type("lF050Auto") == "U"
        If cFunname == "FINA450" .and. Type("lF080Auto") <> "U"
            lF050Auto := .T.
        Else
            lF050Auto:=.F.
        EndIf
    EndIf

    //Evitar o recálculo dos impostos ao alterar somente o vencimento do título que retem impostos somente na emissão
    If lAltera .And. M->E2_VALOR == nOldValor .And. cField $ "M->E2_VALOR|M->E2_VENCREA|M->E2_VENCTO" .And.;
        cFunname $ "FINA050|FINA750" .And. (!lPCCBaixa .Or. M->E2_CSLL + M->E2_COFINS + M->E2_PIS == 0) .And.;
        (!lIRPFBaixa .Or. M->E2_IRRF == 0 ) .And. (!lCalcIssBx .Or. M->E2_ISS == 0)
        Return .T.
    EndIf

    If FwIsInCallStack("FINA631") .And. SuperGetMv("MV_IMPTRAN",.F.,"1") == "2"
        nX := Ascan(aAutoCab, {|e| AllTrim(e[1]) == "E2_VALOR"})
        M->E2_VALOR := aAutoCab[nX][2]
        nX := Ascan(aAutoCab, {|e| AllTrim(e[1]) == "E2_VLCRUZ"})
        M->E2_VLCRUZ := aAutoCab[nX][2]
        nX := Ascan(aAutoCab, {|e| AllTrim(e[1]) == "E2_SALDO"})
        M->E2_SALDO := aAutoCab[nX][2]
    EndIf

    If lEasyFin .And. Type( "n050ValBru" ) == "N" .And. !lVlOnlyRet
        If M->E2_MOEDA == 1
            n050ValBru := M->E2_VALOR            // Variavel criada pelo módulo SIGAEIC, com a função de agregar o valor bruto do título
        Else
            If Empty(M->E2_TXMOEDA)
                n050ValBru := M->E2_VALOR * RecMoeda(M->E2_EMISSAO, M->E2_MOEDA)
            Else
                n050ValBru := M->E2_VALOR * M->E2_TXMOEDA
            EndIf
        EndIf
    EndIf

    If lAltera .and. (cField <> "M->E2_VENCREA")
        //Caso tenha contabilizado, nao posso alterar valores no titulo
        IF SE2->E2_LA = "S"
            Help(" ",1,"NAOVALOR")
            Return( .F. )
        EndIF
        If SE2->E2_TIPO $ MVPAGANT
            Help( " ",1,"FA040ADTO")
            Return( .F. )
        Endif
        If SE2->E2_TIPO $ MVTAXA+"/"+MVINSS+"/"+MVISS+"/"+MVTXA+"/"+"SES"+"/"+"INA"
            Help( " ",1,"F050IMPOST")
            Return( .F. )
        Endif
        If cPaisLoc $ "DOM|COS"
            If 	SE2->E2_TIPO $ MVABATIM
                Help( " ",1,"F050IMPOST")
                Return( .F. )
            EndIf
            If 	SUBSTR(SE2->E2_ORIGEM,1,4) <> "FINA"
                Help( " ",1,"F050IMPOST")
                Return( .F. )
            Endif
        EndIf
    Endif

    //Forcar o posicionamento do fornecedor, que pode entrar na rotina desposicionado
    dbSelectArea("SA2")
    cChaveSA2 := xFilial("SA2") + M->(E2_FORNECE + E2_LOJA)

    If lF050Auto
        cChaveSA2 := xFilial("SA2") + M->E2_FORNECE + SPACE(TamSx3("E2_FORNECE")[1] - LEN(M->E2_FORNECE)) + M->E2_LOJA + SPACE(TamSx3("E2_LOJA")[1] - LEN(M->E2_LOJA))
    EndIf

    SA2->(dbSeek(cChaveSA2))

    lIRPFBaixa := IIf( __lLocBRA, SA2->A2_CALCIRF == "2", .F.)
    __lRateioIR := .F.
    lRatOK	:= .T.
    // Verifica se o fornecedor trata o valor minimo de retencao. 1 - Não considera  2 - Considera o parâmetro MV_VLRETIR
    If __lLocBRA .and. SA2->A2_MINIRF == "2"
        lAplMinIR := .T.
    Endif

    //Alteração de campos que podem influenciar no calculo da base de impostos (baixa)
    If cField $ "M->E2_MOEDA|M->E2_TXMOEDA" .and. lIRPFBaixa
        lCpoValor := .T.
    Endif

    If cField == "M->E2_VALOR" .Or. Empty(cField)
        nValBruto := M->E2_VALOR
        nValDig := M->E2_VALOR
        lCpoValor := .T.
    ElseIf !(cField $ "M->E2_IRRF|M->E2_TXMOEDA|M->E2_INSS|M->E2_SEST|")
        RecompoeVl(lIRPFBaixa, lCalcIssBx, lPCCBaixa)
    Endif

    If SA2->A2_MINPUB == "2"
        lAplMinP := .T.
    EndIF

    If cField == "M->E2_MOEDA" .And. M->E2_MOEDA < 2
        M->E2_TXMOEDA := 0
        nOldTxMoeda := 0
    EndIf

    //motor de retençoes
    If __lTemMR
        F050VldImp(.F.)
    EndIf

    dbSelectArea("SED")

    If !dbSeek(cFilial + M->E2_NATUREZ)
        Return( .T. )
    EndIf

    If cField == "M->E2_CODISS"
        If SED->ED_CALCISS <> "S"
            Return( .T. )
        ElseIf !( M->E2_TIPO $ MVPAGANT ) .and. M->E2_VALOR < nValDig
            //Efetuo a soma dos impostos novamente para não serem descontadas em duplicidade abaixo
            M->E2_VALOR += Iif(!__lIrfMR .And. lIRPFBaixa, 0, M->E2_IRRF) + M->E2_INSS + Iif(!__lIssMR .And. lCalcIssBx, 0, M->E2_ISS)
        EndIf
    EndIf

    lIRProg := IIf(__lLocBRA,IIf(!Empty(SA2->A2_IRPROG),SA2->A2_IRPROG,"2"),"2")

    If (!lF050Auto .And. ( nOldValor == m->e2_valor ) ) .and. ProcName(1) != "F050CALCRT"
        If !lPCCBaixa .And. !M->E2_TIPO $ MVPAGANT .AND. !("M->E2_FRETISS" $ Readvar()) .And. (!lIRPFBaixa .and. !lBaseImp)
            //Caso a chamada tenha sido feita pela rotina Fa050Subst, nao sair
            If Select("__SUBS") == 0
                Return( .T. )
            Endif
        ElseIf  M->E2_TIPO $ MVPAGANT  //Se for PA (geracao de tx's pela emissao), compoe o valor novamente.
            // Caso a funcao Fa050Nat2 tenha sido chamada a partir da alteracao dos campos de Irrf (.T.), Inss e Iss,
            // Fazemos os recalculos dos impostos da lei 10925  sem e fazer a recarga dos valores destes campos.
            If !lVlOnlyRet
                If !__lSestMR
                    m->e2_valor := m->e2_valor +  m->e2_sest
                EndIf

                // Tratamento Moeda Estrangeira
                If M->E2_MOEDA == 1
                    m->e2_valor += If( !__lIrfMR .and. !lIRPFBaixa ,m->e2_irrf , 0)
                    m->e2_valor += If( !__lIssMR .and. !lCalcIssBx ,m->e2_iss  , 0)
                    m->e2_valor += If( !__lInsMR ,m->e2_inss  , 0)
                Else
                    m->e2_valor += If( !__lIrfMR .and. !lIRPFBaixa ,Round(xMoeda(m->e2_irrf,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,nOldTxMoeda),MsDecimais(1)) , 0)
                    m->e2_valor += If( !__lIssMR .and. !lCalcIssBx ,Round(xMoeda(m->e2_iss,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,nOldTxMoeda),MsDecimais(1)) , 0)
                    m->e2_valor += If( !__lInsMR, Round(xMoeda(m->e2_inss,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,nOldTxMoeda),MsDecimais(1)) , 0)

                    // PCC
                    If !__lPccMR
                        nOldpis		:= Round(xMoeda(nOldpis,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,nOldTxMoeda),MsDecimais(1))
                        nOldCofins	:= Round(xMoeda(nOldCofins,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,nOldTxMoeda),MsDecimais(1))
                        nOldCsll	:= Round(xMoeda(nOldCsll,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,nOldTxMoeda),MsDecimais(1))
                    Endif
                EndIf

                If !lPccBaixa .or. (lPccBaixa .and. M->E2_TIPO $ MVPAGANT)
                    If !__lPccMR
                        //Caso seja chamado da alteracao
                        If lAltera
                            //Se reteve PIS - Somo ao valor do titulo
                            If M->E2_PRETPIS == ' '
                                M->E2_VALOR += nOldpis
                            Endif

                            //Se reteve COFINS - Somo ao valor do titulo
                            If M->E2_PRETCOF == ' '
                                M->E2_VALOR += nOldCofins
                            Endif

                            //Se reteve CSLL - Somo ao valor do titulo
                            If M->E2_PRETCSL == ' '
                                M->E2_VALOR += nOldCsll
                            Endif

                            If M->E2_VALOR <> M->E2_BASEPIS
                                M->E2_SALDO += (nOldPis + nOldCofins + nOldCsll)
                            Endif
                        Else //Inclusao
                            If nOldValor <> M->E2_VALOR
                                M->E2_VALOR += nOldpis + nOldCofins + nOldCsll
                            EndIf
                        Endif
                    EndIf
                Endif
            Else
                If M->E2_TIPO $ MVPAGANT .and. !lPaBruto
                    If !__lIrfMR
                        m->e2_valor += m->e2_irrf
                    EndIf
                    If !__lIssMR .And. !lCalcIssBx
                        m->e2_valor += m->e2_iss
                    EndIf
                    If !__lIssMR .and. __lBtrISS
                        m->e2_valor += m->e2_btriss
                    Endif
                    If !__lInsMR
                        m->e2_valor += m->e2_inss
                    EndIf
                Endif
            Endif
        Endif
    EndIf

    //Caso seja um titulo originador de desdobramento.
    If lDsdobra
        Return( .T. )
    Endif

    If lRefImp .And. lTmsOper .And. (FwIsInCallStack('TMSQUITAC') .Or. FwIsInCallStack('TMA250SE2'))
        lBaseImp := .F.
    EndIf

    //Base Impostos diferenciada
    If lBaseImp .and. !lF050Auto
        //Para os casos onde foi alterada a natureza e a nova natureza passa a calcular impostos Alimento a base de impostos
        If M->E2_BASEIRF == 0 .or. lCpoValor
            //Alimento a base de impostos
            If !__lIrfMR .And. (M->E2_BASEIRF == 0 .or. lCpoValor)
                If __lLocBRA .and. SED->ED_IRRFCAR=='S' .and. SED->ED_BASEIRC > 0
                    M->E2_BASEIRF :=	If(__nVlrMR > 0, __nVlrMR, M->E2_VALOR) * (SED->ED_BASEIRC/100)
                Elseif !lIRPFBaixa .and. lBaseDif .and. SED->ED_BASEIRF > 0
                    M->E2_BASEIRF :=	If(__nVlrMR > 0, __nVlrMR, M->E2_VALOR) * (SED->ED_BASEIRF/100)
                Else
                    M->E2_BASEIRF :=	If(__nVlrMR > 0, __nVlrMR, M->E2_VALOR)
                Endif
            Endif
        Endif
        If !__lPccMR
            If M->E2_BASEPIS == 0 .or. lCpoValor
                If !(!lPccBaixa .And. lAltera .And. M->E2_BASEPIS > M->E2_VALOR .And. M->E2_VALOR <= nVlMinImp)
                    M->E2_BASEPIS :=	If(__nVlrMR > 0, __nVlrMR, M->E2_VALOR)
                ElseIf lAltera .and. !lPccBaixa
                    M->E2_BASEPIS := nValDig
                Endif
            Endif

            If M->E2_BASECOF == 0 .or. lCpoValor
                If !(!lPccBaixa .And. lAltera .And. M->E2_BASECOF > M->E2_VALOR .And. M->E2_VALOR <= nVlMinImp)
                    M->E2_BASECOF :=	If(__nVlrMR > 0, __nVlrMR, M->E2_VALOR)
                ElseIf lAltera .and. !lPccBaixa
                    M->E2_BASECOF := nValDig
                Endif
            Endif

            If M->E2_BASECSL == 0 .or. lCpoValor
                If !(!lPccBaixa .And. lAltera .And. M->E2_BASECSL > M->E2_VALOR .And. M->E2_VALOR <= nVlMinImp)
                    M->E2_BASECSL :=	If(__nVlrMR > 0, __nVlrMR, M->E2_VALOR)
                ElseIf lAltera .and. !lPccBaixa
                    M->E2_BASECSL := nValDig
                Endif
            Endif
        EndIf

        If !__lIssMR .And. (M->E2_BASEISS == 0 .or. lCpoValor)
            M->E2_BASEISS :=	If(__nVlrMR > 0, __nVlrMR, M->E2_VALOR)
        Endif

        If !__lInsMR .And. (M->E2_BASEINS == 0 .or. lCpoValor)
            M->E2_BASEINS :=	If(__nVlrMR > 0, __nVlrMR, M->E2_VALOR)
        Endif

        nBaseIrrf   := M->E2_BASEIRF
        nBasePCC	:= M->E2_BASEPIS
        nBaseIns	:= M->E2_BASEINS
        nBaseIss    := M->E2_BASEISS

        If nBasePCC > 0
            M->E2_BASECOF := nBasePCC
            M->E2_BASECSL := nBasePCC
        Endif

        If !__lPccMR .And. M->E2_BASEPIS > 0
            If __nVlrMR > 0
                nBasePCC := IIF ((__nVlrMR <= nVlMinImp), __nVlrMR, M->E2_BASEPIS)
            Else
                nBasePCC := IIF ((M->E2_VALOR <= nVlMinImp), M->E2_VALOR, M->E2_BASEPIS)
            EndIf
        Endif

    ElseIf lF050Auto// Busca base diferencia quando calculo for por ExecAuto, para manter integridade do valor(M->E2_VALOR)
        nX := Ascan(aAutoCab, {|e| AllTrim(e[1]) == "E2_VALOR"})
        If nX > 0 .And. !__lOtImpMR
            M->E2_VALOR := aAutoCab[nX][2]
        Endif

        If F050ImpAut("E2_BASEIRF", @nX)
            nBaseIrrf := aAutoCab[nX][2]
        Else
            If __lLocBRA .and. SED->ED_IRRFCAR=='S' .and. SED->ED_BASEIRC > 0
                M->E2_BASEIRF :=	M->E2_VALOR * (SED->ED_BASEIRC/100)
            Elseif !lIRPFBaixa .and. lBaseDif .and. SED->ED_BASEIRF > 0
                M->E2_BASEIRF :=	M->E2_VALOR * (SED->ED_BASEIRF/100)
            Else
                M->E2_BASEIRF :=	M->E2_VALOR
            Endif
            nBaseIrrf := M->E2_BASEIRF
        Endif

        If F050ImpAut("E2_BASEPIS", @nX)
            nBasePCC := aAutoCab[nX][2]
            M->E2_BASEPIS := nBasePCC
        Else
            M->E2_BASEPIS :=	M->E2_VALOR
            nBasePCC := M->E2_VALOR
        Endif

        If F050ImpAut("E2_BASECOF", @nX)
            nBasePCC := aAutoCab[nX][2]
            M->E2_BASECOF := nBasePCC
        Else
            M->E2_BASECOF :=	M->E2_VALOR
            nBasePCC := M->E2_VALOR
        Endif

        If F050ImpAut("E2_BASECSL", @nX)
            nBasePCC := aAutoCab[nX][2]
            M->E2_BASECSL := nBasePCC
        Else
            M->E2_BASECSL :=	M->E2_VALOR
            nBasePCC := M->E2_VALOR
        Endif

        If F050ImpAut("E2_BASEISS", @nX)
            nBaseIss := aAutoCab[nX][2]
        Else
            M->E2_BASEISS :=	M->E2_VALOR
            nBaseIss := M->E2_VALOR
        Endif

        If F050ImpAut("E2_BASEINS", @nX)
            nBaseIns := aAutoCab[nX][2]
        Else
            M->E2_BASEINS :=	M->E2_VALOR
            nBaseIns := M->E2_VALOR
        Endif
    Endif

    // Caso a funcao Fa050Nat2 tenha sido chamada a partir da alteracao dos campos de Irrf (.T.), Inss e Iss,
    // Fazemos os recalculos dos impostos da lei 10925  sem e fazer a recarga dos valores destes campos.
    If !lVlOnlyRet

        IF !__lIssMR .And. ( !lF050Auto .Or. !F050ImpAut("E2_ISS") ) .And. SED->ED_CALCISS == "S" // Natureza Cálcula ISS

            // Data de Referencia
            dRefISS := IF( lRetISSEmi, M->E2_EMISSAO, M->E2_VENCREA)

            // Base de Cálculo
            nBaseIss := Round(xMoeda(nBaseIss, M->E2_MOEDA, 1, M->E2_EMISSAO, MsDecimais(1)+1, M->E2_TXMOEDA), MsDecimais(1))

            // Percentual da Nota
            If !lFINA050 .and. Alltrim(SE2->E2_ORIGEM) == "MATA100" // D1_FILIAL+D1_DOC+D1_SERIE+D1_FORNECE+D1_LOJA+D1_COD+D1_ITEM
                nPercIss := Posicione("SD1", 1, xFilial("SD1") + SE2->E2_NUM + SE2->E2_PREFIXO + SE2->E2_FORNECE + SE2->E2_LOJA , "D1_ALIQISS" )
            EndIf

            // Cálculo do ISS
            aCalcIss := FCalcISS( "P", dRefISS, nBaseIss, nPercIss, M->E2_FORNECE, M->E2_LOJA, M->E2_FRETISS == "1", M->E2_CODISS, M->E2_FILORIG)

            nCalcIss := aCalcIss[1][1]
            nRetIss  := aCalcIss[1][3]
            nBasRIss := aCalcIss[2][3]

            If __lBtrISS	// Bitributacao de ISS
                aISSCPOM := ISSCPOM("T", SA2->(A2_COD+A2_LOJA), M->E2_CODSERV)
                If !Empty(aISSCPOM)
                 	If SA2->A2_RECISS <> "S"
	                    If lRndVlIss
	                        m->e2_btriss := Round(((nBaseIss) * aISSCPOM[3] / 100),2)
	                    Else
	                        m->e2_btriss := NoRound(((nBaseIss) * aISSCPOM[3] / 100),2)
	                    EndIf
                    Else
                    	lCPRecISS := .T.
                    EndIf
                Else
                    m->e2_btriss:= 0
                    nOldBtrISS 	:= 0
                EndIf
            EndIf

            // Verifica se Natureza pede calculo do ISS (FORNECEDOR NŽAO RECOLHE)
            // e se nao ‚ titulo Provisorio ou Adiantamento ou Abatimento
            If ( SA2->A2_RECISS == "S" .And. !lCPRecISS ) .Or. M->E2_TIPO $ MVABATIM+"/"+MVPROVIS+"/"+MVTAXA+"/"+MVINSS+"/"+MVISS+"/"+MVTXA +"/"+"SES"+"/"+MV_CPNEG+"/"+"INA"
                M->E2_VALOR += Round(xMoeda( M->E2_ISS + IF(__LBTRISS, M->E2_BTRISS, 0),1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA), 2)

                If m->e2_tipo == MVPAGANT .AND. lPrImPA
                    If SED->ED_CALCISS == "S" .AND. SA2->A2_RECISS != "S" .AND. !lCalcIssBx
                        M->E2_PRISS := nCalcIss
                    EndIf
                EndIf

                M->E2_ISS := 0
                nOldISS	  := 0
            ElseIf SA2->A2_RECISS <> "S"
            	lCPRecISS := .T.
            EndIf

            If lCPRecISS

            	lTitRetISS := .T.
            	If M->E2_TIPO $ MVPAGANT
            		If !lCalcIssBx
            			lTitRetISS := .F.
            			If lPrImPA
	            			M->E2_PRISS := nRetIss
                        EndIf
                        M->E2_ISS := 0
                        nOldISS := 0
                        If __lBtrISS
                            M->E2_BTRISS := 0
                            nOldBtrISS 	 := 0
                        EndIf
	                EndIf
            	Else
            		If lPrImPA .And. !lCalcIssBx
            			M->E2_VALOR += Round(xMoeda(M->E2_PRISS,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA), 2)
            			M->E2_PRISS := 0
            		EndIf
            	EndIf

            	If lTitRetISS
            		M->E2_ISS := nRetIss
		            M->E2_VBASISS := Round(xMoeda(nBasRIss,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA), 2)
                    If  M->E2_TIPO $ MVPAGANT .Or. !lCalcIssBx
                        M->E2_VRETISS := nCalcIss
                    EndIf
		        EndIf

                //-- Ajusta o ISS qdo a chamada for efetuada atraves do contrato de carreteiro.
                If Left(FunName(),7) == 'TMSA250' .Or. Left(FunName(),7) == 'TMSA251'
                    If Type("cNumCTC") <> "U"
                        TMA250ISS()
                    EndIf
                    //Verifica se a rota e municipal, se nao for nao gera titulo de ISS, zerando o campo
                    If cTipUso == "1" .And. Type("cFilVge") <> "U" .And. Type("cNumVge") <> "U"
                        DTQ->(DbSetOrder(2))
                        If DTQ->(MsSeek(xFilial("DTQ")+cFilVge+cNumVge))
                            DA8->(DbSetOrder(1))
                            If __lLocBRA .And. DA8->(MsSeek(xFilial("DA8")+DTQ->DTQ_ROTA))
                                If DA8->DA8_ROTMUN == StrZero(2,Len(DA8->DA8_ROTMUN)) //Rota Municipal = Nao
                                    M->E2_ISS := 0
                                EndIf
                            EndIf
                        EndIf
                    EndIf
                EndIf

            EndIf

            If __lBtrISS .And. lVerMinIss .And. !Empty(aISSCPOM) .And. Upper(AllTrim(aISSCPOM[2])) == Upper(AllTrim(SM0->M0_CIDENT))
                nRetIss  := 0
                nVRetISS := 0
                SomaTitISS("SE2",M->E2_FORNECE,M->E2_LOJA,M->E2_VENCREA,@nRetIss,@nVRetISS,@nTotBasISS,nBaseISS,,__lBtrISS)
                If M->E2_BTRISS + nRetIss <= aISSCPOM[4]
                    M->E2_VRETBIS := M->E2_BTRISS
                    M->E2_BTRISS := 0
                EndIf
            EndIf

        EndIf

        //CALCULO INSS
        If !__lInsMR .And. ( !lF050Auto .or. !F050ImpAut("E2_INSS") )

            // Verifica se Natureza pede calculo do INSS (RECOLHE INSS P/ FORNEC) e se não ‚ titulo Provisorio ou Adiantamento ou Abatimento
            If M->E2_TIPO $ MVPAGANT + "/" + MVPROVIS + "/" + MVTAXA + "/" + MVINSS + "/" + MV_CPNEG + "/" + MVABATIM + "/SES/INA"

                If SED->ED_DEDINSS $ "1 " .And. (M->E2_INSS == 0 .OR. M->E2_INSS == (M->E2_BASEINS * SED->ED_PERCINS) )
                    m->e2_valor += NoRound(m->e2_inss,2)
                Endif
                m->e2_inss  := 0
                nOldInss    := 0
                m->e2_valor += Iif(lRndSest,Round(m->e2_sest,2),NoRound(m->e2_sest,2))
                m->e2_sest  := 0
                nOldSEST    := 0

                If M->E2_TIPO $ MVPAGANT .And. !lInsPub
                    If SED->ED_CALCINS == "S" .and. SA2->A2_RECINSS == "S"
                        nIrfInss := 0

                        If !Empty(SED->ED_BASEINS)
                            nBaseIns := NoRound((M->E2_VALOR * (SED->ED_BASEINS/100)),2)
                            M->E2_BASEINS:= nBaseIns
                        ElseIf nBaseIns == 0
                            nBaseIns := M->E2_VALOR
                        EndIf
                        nBaseIns := xMoeda(nBaseIns, M->E2_MOEDA, 1, M->E2_EMISSAO, 3, M->E2_TXMOEDA)

                        If SA2->A2_TIPO == "F" //Para pessoa fisica verifico o limite de deducao no mes
                            nValInss := FCalcInsPF(nBaseIns, @nCalcInss, @nINSSTot)
                        Else
                            nValInss := FCalcInsPJ(nBaseIns, @nCalcInss, @nINSSTot)
                        Endif

                        If __lLocBRA .And. SED->ED_RINSSPA == "1" //retem o valor no INSS em titulo tipo INA
                            M->E2_INSS    := Max(nINSSTot,nValInss)
                            M->E2_PRINSS  := 0
                            M->E2_VRETINS := nCalcInss

                            //-- Valor do titulo nao pode ser menor que o valor do INSS
                            If M->E2_VALOR < M->E2_INSS
                                M->E2_INSS := M->E2_VALOR - 0.01
                            EndIf

                            IF lF050INBR // Ponto de entrada para calculo de INSS com base reduzida
                                M->E2_INSS := ExecBlock("F050INBR",.f.,.f.,M->E2_VALOR)
                                nVCalINS := M->E2_INSS
                                nBCalINS := nBaseIns
                            Endif
                        ElseIf lPrImPa
                        	M->E2_PRINSS := Max(nINSSTot,nValInss)
                        EndIf

                        nInss	 := nValInss
                        nVCalINS := nValInss
                        nBCalINS := nBaseIns
                    EndIf
                EndIf

            Else

                If SED->ED_CALCINS == "S" .and. SA2->A2_RECINSS == "S"
                    nIrfInss := 0
                    If !Empty(SED->ED_BASEINS)
                        nBaseIns := NoRound((M->E2_VALOR * (SED->ED_BASEINS/100)),2)
                        E2_BASEINS:= nBaseIns
                    ElseIf nBaseIns == 0
                        nBaseIns := M->E2_VALOR
                    EndIf
                    nBaseIns := xMoeda(nBaseIns, M->E2_MOEDA, 1, M->E2_EMISSAO, 3, M->E2_TXMOEDA)

                    If SA2->A2_TIPO == "F" //Para pessoa fisica verifico o limite de deducao no mes
                        nValInss := FCalcInsPF(nBaseIns, @nCalcInss, @nINSSTot)
                    Else
                        nValInss := FCalcInsPJ(nBaseIns, @nCalcInss, @nINSSTot)
                    Endif

                    M->E2_INSS := Max(nINSSTot,nValInss)
                    M->E2_VRETINS := nCalcInss

                    //-- Valor do titulo nao pode ser menor que o valor do INSS
                    If M->E2_VALOR < M->E2_INSS
                        M->E2_INSS  := M->E2_VALOR - 0.01
                    EndIf
                    nVCalINS := M->E2_INSS
                    nBCalINS := nBaseIns

                    // Ponto de entrada para calculo de INSS com base reduzida
                    IF lF050INBR
                        M->E2_INSS := ExecBlock("F050INBR",.f.,.f.,M->E2_VALOR)
                        nVCalINS := M->E2_INSS
                        nBCalINS := nBaseIns
                    Endif
                Endif

            Endif

            If M->E2_INSS == 0 .And. ((SA2->A2_TIPO == "J" .And. lAcmPJ) .Or. (SA2->A2_TIPO == "F"))
                M->E2_PRETINS := "1"
            ElseIf M->E2_INSS > 0  .And. SA2->A2_TIPO == "F"
                M->E2_PRETINS := " "
            Endif

            //SEST
            If SED->ED_CALCSES == 'S' .And. SA2->A2_RECSEST == "1"
                If !Empty(SED->ED_BASESES)
                    nValSEST := Iif(lRndSest,Round((m->e2_valor * (SED->ED_BASESES/100)),2),NoRound((m->e2_valor * (SED->ED_BASESES/100)),2))
                Else
                    nValSEST := M->E2_VALOR
                EndIf
                m->e2_sest := Iif(lRndSest,Round((nValSEST * (SED->ED_PERCSES/100)),2),NoRound((nValSEST * (SED->ED_PERCSES/100)),2))
                nValSEST := m->e2_sest
            Endif
        Endif

        //CIDE
		If lCIDE .And. !__lCidMR .And. !(M->E2_TIPO $ MVABATIM+"/"+MVPROVIS+"/"+MVTAXA+"/"+MVINSS+"/"+MVISS+"/"+MVTXA +"/"+"SES"+"/"+MV_CPNEG+"/"+"INA")

            nBaseCide := M->E2_VALOR

            If M->E2_MOEDA > 1
                nBaseCide := Round(xMoeda(nBaseCide,M->E2_MOEDA,1,M->E2_EMISSAO,TamSX3("E2_TXMOEDA")[2],M->E2_TXMOEDA,1),2)
            EndIf

            M->E2_CIDE := FCalcCIDE(nBaseCide, M->E2_NATUREZ, M->E2_FORNECE, M->E2_LOJA)
        Else
            M->E2_CIDE := 0
        EndIf

        //IRRF
        If !__lIrfMR .And. ( !lF050Auto .or. !F050ImpAut("E2_IRRF") )
            // Verifica se Natureza pede calculo do IRRF e se n†o ‚
            // titulo Provisorio ou Adiantamento ou Abatimento
            If SED->ED_CALCIRF == "N" .or. m->e2_tipo $ MVABATIM+"/"+MVPROVIS+"/"+MVTAXA+"/"+MVINSS+"/"+MVISS+"/"+MVTXA +"/"+"SES"+"/"+MV_CPNEG+"/"+"INA" .or. ;
                (m->e2_tipo $ MVPAGANT .and. GetMv("MV_IMPADT") != "S")
                m->e2_valor += NoRound(m->e2_irrf,2)
                m->e2_irrf	:= 0
                nOldIrr		:= 0
            Else
                //Caso o titulo de carreteiro seja incluido pelo modulo Financeiro e no cadastro de fornecedor esteja
                //configurado para executar calculo de IRRF na baixa do titulo, o mesmo sera executado no fonte FINA241
                If SED->ED_IRRFCAR == "S" .And. IIf(nModulo != 43,!lIRPFBaixa,.T.)
                    //Verifico a combinacao de filiais (SM0) e lojas de fornecedores a serem considerados
                    //na montagem da base do IRRF
                    If cAglImPJ != "1"
                        aRet := FLOJASIRRF("2")
                        aFilial := aClone(aRet[1])
                        aCliFor := aClone(aRet[2])
                        cArqTMP := aRet[3]
                    Endif
                    // Verifica se Pessoa Fisica ou Juridica, para fins de calculo do irrf
                    If lF050ATP
                        lAplicaTP := ExecBlock("F050ATP",.F.,.F.)
                    Endif
                    //-- Se o parametro MV_TPTCTC nao estiver preenchido
                    If Empty(cTipCTC)
                        cTipCTC := Padr( "C" + cFilAnt, Len( SE2->E2_TIPO ) ) // Tipo Contrato de Carreteiro
                    EndIf
                    If (SA2->A2_TIPO == "F" .OR. (SA2->A2_TIPO == "J" .AND. lIRProg == "1")) .AND. !lEmprInd .AND. lAplicaTP

                        //--Regra para calculo do imposto de renda - Pessoa Fisica:

                        //--Devem ser somados -TODOS- os titulos que com data de vencimento
                        //--no mes corrente.
                        //--Apos isto, deve-se aplicar a reducao de base de calculo do IR (Se houver) e
                        //--deduzir o valor acumulado de INSS retido.
                        //--Em seguida, verificar em qual faixa da tabela progressiva do IRRF se enquadra o
                        //--valor obtido.
                        //--Aplicar a aliquota do imposto, deduzir o valor referente a faixa
                        //--da tabela progressiva e abater os impostos retidos (IRRF) anteriores.

                        //--Exemplo:

                        //--Tabela Progressiva do IRRF (Exemplo):
                        //--  Ate(R$)   Aliq.    Val. Deduzir
                        //-- 1.434,59	  0,0	         0,00
                        //-- 2.150,00	  7,5	       107,59
                        //-- 2.866,70	 15,0	       268,84
                        //-- 3.582,00	 22,5	       483,84
                        //-- 9.999,99	 27,5	       662,94

                        //-- Titulo c/       Valor          Base     Valor
                        //-- Vencto. em:     Titulo      Calculo      IRRF
                        //--    01/08/09   1.000,00     1.000,00      0,00 (Alcancou a 1.a Faixa)
                        //--    15/08/09   2.000,00     3.000,00    191,16 (Alcancou a 4.a Faixa) Formula: ((3000 * 22.5)/100)-483.84)
                        //--    31/08/09   2.000,00     5.000,00    520,90 (Alcancou a 5.a Faixa) Formula: ((5000 * 27,5)/100)-(662,94+191,16)
                        //--    31/08/09   1.000,00     6.000,00    275,00 (Alcancou a 5.a Faixa) Formula: ((6000 * 27,5)/100)-(662,94+191,16+520,90)

                        //-- Se houver reducao de base de calculo (40% de reducao):
                        //-- Titulo c/       Valor         Valor        Base     Valor
                        //-- Vencto. em:     Titulo    Acumulado     Calculo      IRRF
                        //--    01/08/09   1.000,00     1.000,00      400,00       0,00 (Alcancou a 1.a Faixa)
                        //--    15/08/09   2.000,00     3.000,00    1.600,00      12,41 (Alcancou a 2.a Faixa) Formula: ((1600 * 7,5)/100)-107,59)
                        //--    31/08/09   2.000,00     5.000,00    2.000,00      30,00 (Alcancou a 2.a Faixa) Formula: ((2000 * 7,5)/100)-(107,59+12,41)
                        //--    31/08/09   1.000,00     6.000,00    2.400,00      48,75 (Alcancou a 3.a Faixa) Fomrula: ((2400 * 15)/100)-(268,84+12,41+30)

                        //--Obtem os titulos com vencimento no periodo:
                        cAliasQry := GetNextAlias()
                        cQuery := "SELECT SE2.E2_FILIAL, SE2.E2_PREFIXO, SE2.E2_NUM, SE2.E2_PARCELA, "
                        cQuery += "SE2.E2_TIPO, SE2.E2_FORNECE, SE2.E2_LOJA, SE2.E2_IRRF, "
                        cQuery += "SE2.E2_INSS, SE2.E2_VENCREA, SE2.E2_VALOR, SE2.E2_BAIXA, "
                        cQuery += "SE2.E2_FATURA, SE2.E2_SEST, SE2.E2_BASEIRF, SE2.E2_ORIGEM, SE2.E2_NATUREZ,SE2.E2_ISS, "
                        cQuery += "SE2.E2_VRETIRF, SE2.E2_STATUS, SE2.E2_FILORIG "

                        If __lBtrISS
                            cQuery += " ,SE2.E2_BTRISS "
                        EndIf

                        cQuery += "FROM " + RetSQLTab('SE2')

                        cQuery += "JOIN " + RetSQLTab('SED')
                        cQuery += "ON  SED.ED_FILIAL  = '" + xFilial('SED') + "' AND "
                        cQuery += "SED.ED_CODIGO  = SE2.E2_NATUREZ AND "
                        cQuery += "SED.ED_CALCIRF = 'S' AND "
                        If !lFina050
                            cQuery += "SED.ED_IRRFCAR = 'S' AND "
                        Endif
                        cQuery += "SED.D_E_L_E_T_ = ' ' "

                        cQuery += " WHERE "
                        
                        //Se verifica base apenas na filial corrente e fornecedor corrente
                        If cAglImPJ == "1" .Or. Empty( cFilFwSE2 )
                            cQuery += "SE2.E2_FILIAL = '"+ xFilial("SE2") + "' AND "

                            If cAglImPJ == "1" 				//Verificar apenas fornecedor corrente
                                cQuery += "SE2.E2_FORNECE = '"+ SA2->A2_COD +"' AND "
                                cQuery += "SE2.E2_LOJA = '"+ SA2->A2_LOJA +"' AND "
                            Else									//Verificar determinados fornecedores (raiz do CNPJ)
                                cQuery += " (E2_FORNECE||E2_LOJA IN (SELECT CODIGO||LOJA FROM " + cArqTMP + ")) AND "
                            Endif

                        ElseIf Len(aFilial) > 0  //Mais de uma filial SM0

                            If Empty( cFilFwSA2 )  //Se cadastro de Clientes compartilhado
                                cQuery += "SE2.E2_FILIAL IN ( "
                                For nLoop := 1 to Len(aFilial)
                                    cQuery += "'"  + aFilial[nLoop] + "',"
                                Next
                                //Retiro a ultima virgula
                                cQuery := Left( cQuery, Len( cQuery ) - 1 )
                                cQuery += ") AND "

                                //Verificar determinados fornecedores (raiz do CNPJ)
                                cQuery += " (E2_FORNECE||E2_LOJA IN (SELECT CODIGO||LOJA FROM "+cArqTMP+")) AND "
                            Else							//Se cadastro de Clientes EXCLUSIVO
                                cQuery += " (E2_FILIAL||E2_FORNECE||E2_LOJA IN (SELECT FILIALX||CODIGO||LOJA FROM "+cArqTMP+")) AND "
                            Endif
                        Endif

                        // Para Pessoa fisica totaliza os titulos emitidos no mes
                        If cVenctoPF == "2"
                            cQuery += "      SE2.E2_VENCREA  BETWEEN '" + Dtos(FirstDay(M->E2_VENCREA)) + "' AND '" + Dtos(LastDay(M->E2_VENCREA))+ "' AND "
                        ElseIf cVenctoPF == "1"
                            cQuery += "      SE2.E2_EMISSAO  BETWEEN '" + Dtos(FirstDay(M->E2_EMISSAO)) + "' AND '" + Dtos(LastDay(M->E2_EMISSAO))+ "' AND "
                        ElseIf cVenctoPF == "3"
                            cQuery += "      SE2.E2_EMIS1  BETWEEN '" + Dtos(FirstDay(dDataBase)) + "' AND '" + Dtos(LastDay(dDataBase))+ "' AND "
                        Endif
                        cQuery += " SE2.E2_TIPO NOT IN " + F050TipoIN( ,.T.) 	  + " AND "
                        cQuery += " SE2.E2_FATURA NOT IN('NOTFAT') AND "
                        cQuery += " SE2.E2_STATUS <> 'D' AND "  //Desconsidera os titulos geradores de desdobramento
                                                
                        If lAltera
                            cQuery += " SE2.R_E_C_N_O_ <> " + Alltrim(Str(SE2->(Recno()))) + " AND " //Desconsidera o titulo em alteração na Base.
                        EndIf
                        
                        cQuery += " SE2.D_E_L_E_T_ = ' ' "

                        cQuery := ChangeQuery(cQuery)

                        //--Processa a query e adequa os campos
                        DbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAliasQry,.T.,.T.)

                        //--Confronta os titulos obtidos com os contratos:
                        If !(cAliasQry)->( Eof() )

                            TcSetField(cAliasQry,"E2_IRRF","N",TamSX3("E2_IRRF")[1],TamSX3("E2_IRRF")[2])
                            TcSetField(cAliasQry,"E2_INSS","N",TamSX3("E2_INSS")[1],TamSX3("E2_INSS")[2])
                            TcSetField(cAliasQry,"E2_VALOR","N",TamSX3("E2_VALOR")[1],TamSX3("E2_VALOR")[2])
                            TcSetField(cAliasQry,"E2_VENCREA","D",TamSX3("E2_VENCREA")[1],TamSX3("E2_VENCREA")[2])

                            While !(cAliasQry)->( Eof() )
                                //--Titulo gerado a partir de outra origem, exemplo:
                                //--Pedagio, Adiantamentos, inclusao manual, etc...
                                If lFina050 .AND.  ("FINA" $ (cAliasQry)->E2_ORIGEM .or. "MATA" $ (cAliasQry)->E2_ORIGEM)
                                    nValFrete += (cAliasQry)->E2_BASEIRF
                                    If lDedIns .and. "MATA" $ (cAliasQry)->E2_ORIGEM
                                        nValFrete += (cAliasQry)->E2_INSS
                                    EndIf 
                                Else
                                    nValFrete += (cAliasQry)->E2_VALOR + (cAliasQry)->E2_IRRF + (cAliasQry)->E2_INSS +;
                                        (cAliasQry)->E2_ISS + If(__lBtrISS,(cAliasQry)->E2_BTRISS,0) + (cAliasQry)->E2_SEST //Recompoe o valor
                                Endif
                                
                                //Valida se esta alterando contas a pagar
                                If M->(E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA) != ;
                                   (cAliasQry)->(E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_FORNECE+E2_LOJA)
                                    nValIRRF  += (cAliasQry)->E2_IRRF
                                Endif

                                If lFina050
                                    aAreaSED := SED->(GetArea())
                                    dbSelectArea("SED")
                                    dbSeek(XFILIAL("SED")+(cAliasQry)->E2_NATUREZ)

                                    If !lDedIns .and. SED-> ED_IRRFCAR== "S"
                                        nINSSRet+= (cAliasQry)->E2_INSS
                                    Elseif lDedIns
                                        nINSSRet += (cAliasQry)->E2_INSS
                                    Endif
                                    RestArea(aAreaSED)
                                Else
                                    nINSSRet  += (cAliasQry)->E2_INSS
                                Endif
                                (cAliasQry)->( DbSkip() )
                            EndDo
                        EndIf
                        (cAliasQry)->(DbCloseArea())

                        //Fecha arquivo temporario
                        If cAglImPJ != "1" .and. lDelTrbIR
                            If InTransact()
                                StartJob( "DELTRBIR" , GetEnvServer() , .T. , SM0->M0_CODIGO, IIf( __lCodFil, FWGETCODFILIAL, SM0->M0_CODFIL ),.T.,ThreadID(),cArqTmp)
                            Else
                                DELTRBIR(SM0->M0_CODIGO, IIf( __lCodFil, FWGETCODFILIAL, SM0->M0_CODFIL ),.F.,0,cArqTmp)
                            Endif
                        Endif

                        dbSelectArea("SE2")

                        //--Agrega o valor do titulo que
                        //--esta sendo gerado no momento:
                        If lIsLotePLS
                        	nValFrete  +=  IIF( nBaseIrrf > 0, nBaseIrrf, M->E2_VALOR)
                        else
	                        If __lLocBRA .AND. lFina050 .and. lBaseImp .and. SED->ED_BASEIRC > 0
	                            nValFrete += ((M->E2_VALOR * SED->ED_BASEIRC) / 100)
	                        ElseIf !lParcela
	                            nValFrete  += M->E2_VALOR
	                        Endif
                        endIf

                        // Aplica a reducao de base de calculo (Se houver)
                        If __lLocBRA .and. SED->ED_BASEIRC > 0 .and. !lFina050
                            nValFrete := ((nValFrete * SED->ED_BASEIRC) / 100)
                        EndIf

                        nBasOrig := nValFrete //Guarda a base sem as deduções legais (para o calculo do IRPF simplificado)

                        // Deduz o INSS do IRRF (Carreteiro)
                        nINSSRet += M->E2_INSS
                        If nLimInss > 0 .And. nINSSRet > nLimInss //Verifica se o valor do INSS ultrapassou o valor limite.
                            nINSSRet := nLimInss
                        EndIf

                        If lDedIns
                            nValFrete := nValFrete - nINSSRet
                        EndIf

                        // Deduz os Dependentes
                        nValDep := nBaseDep * SA2->A2_NUMDEP
                        nValFrete -= nValDep

                        If !lAltVcto .AND. !(ALLTRIM(M->E2_ORIGEM) == "FINA290" .AND. (Alltrim(M->E2_FATURA) == "NOTFAT"))
                            //Calculo IRPF considerando deduções legais
                            nCalcIr := NoRound(FA050TabIR(nValFrete) - nValIRRF,2)
                            
                            //Calculo do IRPF considerando dedução simplificada (MP 1.171/23)
                            If cPaisLoc=="BRA" .And. lIrTabSimp .And. M->E2_EMISSAO >= dVigMP1171 .And. nCalcIr > 0
                                nVrIrDedS := NoRound(FA050TabIR(nBasOrig,,lIrTabSimp) - nValIRRF,2)
                                If nCalcIr > nVrIrDedS 
                                    nCalcIr := nVrIrDedS //Considera o IRRF c/ dedução simplificada por ser mais vantajoso
                                    __lDedSimpl := .T.
                                EndIf
                            EndIf

                            M->E2_IRRF := nCalcIr

                        EndIf

                        IF lF050CIRF
                            M->E2_IRRF := ExecBlock("F050CIRF",.f.,.f.,nBaseIrrf)
                        Endif

                    Else
                        If GetNewPar("MV_RNDIRF",.F.)
                            M->E2_IRRF := Round(((xMoeda(M->E2_VALOR,M->E2_MOEDA,1,M->E2_EMISSAO,MsDecimais(1)+1, M->E2_TXMOEDA),MsDecimais(1)) * IF(SED->ED_PERCIRF > 0,SED->ED_PERCIRF,GetMV("MV_ALIQIRF"))/100)-nValIRRF,2)
                        Else
                            M->E2_IRRF := NoRound(((xMoeda(M->E2_VALOR,M->E2_MOEDA,1,M->E2_EMISSAO,MsDecimais(1)+1, M->E2_TXMOEDA),MsDecimais(1)) * IF(SED->ED_PERCIRF > 0,SED->ED_PERCIRF,GetMV("MV_ALIQIRF"))/100)-nValIRRF,2)
                        Endif
                    EndIf

                    //-- Valor do titulo nao pode ser menor que o valor do IRRF
                    If m->e2_valor < m->e2_irrf
                        m->e2_irrf  := m->e2_valor - 0.01
                    EndIf

                    // Regras para retenção do IRRF de Carreteiro.
                    If lAplMinIR .AND.;                     // Fornecedor avalia parâmetro SA2->A2_MINIRF == "2"
                        EMPTY(nValIRRF) .AND.;              // Ainda Não possui retenção no período atual
                        (M->E2_IRRF <= GetMv("MV_VLRETIR")) // O valor apurado eh menor que o parametrizado
                        M->E2_IRRF := 0
                    EndIf
                Else
                    //Base reduzida de impostos
                    //Caso nao existra tratamento, a base sera o valor
                    If !lBaseImp
                        nBaseIrrf := m->e2_valor
                    Endif

                    nCalcIr	:=	0

                    //Se controla base reduzida de IRRF
                    //Se % base maior que 0
                    //Se Fornecedor for pessoa fisica
                    If lIRPFBaixa

                        m->e2_irrf := 0

                        //Verificar Base de IRPF
                        If lBaseIRPF
                            M->E2_BASEIRF := Round(NoRound( xMoeda(nBaseIrrf,M->E2_MOEDA,1,M->E2_EMISSAO,MsDecimais(1)+1, M->E2_TXMOEDA),MsDecimais(1)+1),MsDecimais(1))
                            If lBaseDif .and. SED->ED_BASEIRF > 0
                                M->E2_BASEIRF := M->E2_BASEIRF * (SED->ED_BASEIRF/100)
                            Endif
                        ElseIf __lLocBRA .and. M->E2_BASEIRF > 0 .and. SA2->A2_TIPO == "F"
                            M->E2_BASEIRF := 0
                        Endif
                    Endif
                    // Verifica se Pessoa Fisica ou Juridica, para fins de
                    // calculo do irrf
                    If lF050ATP
                        lAplicaTP := ExecBlock("F050ATP",.F.,.F.)
                    Endif
                    IF (SA2->A2_TIPO == "F" .OR. (SA2->A2_TIPO == "J" .AND. lIRProg == "1")) .AND. lAplicaTP .AND. !Empty(SA2->A2_CALCIRF)
                        If lIRPFBaixa
                            If M->E2_TIPO $ MVPAGANT

                                //Converto para moeda corrente para calcular o IRRF na baixa
                                nBaseIrrf     := Round(NoRound(xMoeda(nBaseIrrf,M->E2_MOEDA,1,M->E2_EMISSAO,MsDecimais(1)+1),MsDecimais(1)+1),MsDecimais(1))
                                M->E2_IRRF    := FCalcIRBx(nBaseIrrf,SA2->A2_TIPO)
                                M->E2_VRETIRF := M->E2_IRRF
                                nVCalIRF      := M->E2_IRRF
                                nBCalIRF      := nBaseIrrf
                            EndIf
                        Else
                            If (!lAltVcto .Or. lF050Auto) .AND. !(ALLTRIM(M->E2_ORIGEM) $ "FINA290#FINA290M"  .AND. (Alltrim(M->E2_FATURA) == "NOTFAT"))
                                nCalcIr := FCalcIr(nBaseIrrf,"F",.T.)

                                If (nCalcIr == 0 .or. (nCalcIr > 0 .and. Recmoeda(M->E2_EMISSAO,M->E2_MOEDA) > 0) ).And.;
                                    M->E2_IRRF > 0 .And. !(m->e2_tipo $ MVPAGANT) .And. M->E2_MOEDA > 1 .And. ;
                                    !lIRPFBaixa  .And. M->E2_TXMOEDA == 0 .And. !(cField $ "M->E2_TXMOEDA|M->E2_VALOR")

                                    M->E2_VALOR	+= Round(xMoeda(M->E2_IRRF,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA),MsDecimais(1))
                                Endif
                                If nCalcIr > 0 .And. (M->E2_IRRF == 0 .or. (M->E2_IRRF > 0 .and. lCpoValor) ).And. !(m->e2_tipo $ MVPAGANT) .And. M->E2_MOEDA > 1 .And. !lIRPFBaixa
                                    M->E2_VALOR	:=	M->E2_VALOR - (Round(xMoeda(nCalcIr,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA),MsDecimais(1)))
                                    lJaDescIr := .T.
                                Endif
                                If M->E2_MOEDA == 1 .And. cField == "M->E2_MOEDA" .And. M->E2_IRRF > 0 .And. nCalcIr > 0 .And. !(m->e2_tipo $ MVPAGANT) .And. !lIRPFBaixa
                                    M->E2_VALOR += nCalcIr
                                    M->E2_TXMOEDA := 0
                                EndIf
                                nValIrOld := M->E2_IRRF
                                lSumIR := M->E2_IRRF == nCalcIr
                                M->E2_IRRF := nCalcIr
                            EndIf
                        EndIf
                    ElseIf !lSimples
                        If lIRPFBaixa
                            aAreaSA2 := SA2->(GetArea())

                            nBasM2Irf := If(M->E2_TIPO $ MVPAGANT, M->E2_VALOR, nBaseIrrf)

                            If !lAltera .And. M->E2_MOEDA > 1
                                nBasM2Irf := xMoeda(nBaseIrrf, M->E2_MOEDA, 1, M->E2_EMISSAO, MsDecimais(1)+1, M->E2_TXMOEDA)
                            EndIf

                            If M->E2_TIPO $ MVPAGANT
                                M->E2_IRRF := FCalcIRBx(nBasM2Irf, SA2->A2_TIPO)
                                M->E2_VRETIRF := M->E2_IRRF
                            Else
                                M->E2_IRRF := FCalcIr(nBaseIrrf,"J",.T.,.T.)
                            EndIf
                            RestArea(aAreaSA2)

                        ElseIf !(ALLTRIM(M->E2_ORIGEM) == "FINA290" .AND. (Alltrim(M->E2_FATURA) == "NOTFAT")) .and. (SA2->(Columnpos("A2_CALCIRF")) > 0 .and. SA2->A2_CALCIRF <> " ")

                            nCalcIr	:=	FCalcIr(nBaseIrrf,"J",.T.,@lIrfRetAnt)

                            If nCalcIr > 0 .And. (M->E2_IRRF == 0 .or. (M->E2_IRRF > 0 .and.  (!lAltTxMoeda .And. lCpoValor ))) .And. !(m->e2_tipo $ MVPAGANT) .And. M->E2_MOEDA > 1 .And. !lIRPFBaixa .And. M->E2_TXMOEDA > 1
                                M->E2_VALOR	:=	M->E2_VALOR - (xMoeda(nCalcIr,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA) )
                                lJaDescIr := .T.
                            Endif

                            lSumIR := M->E2_IRRF == nCalcIr
                            If nCalcIr == 0
                                nIrrfAnt := M->E2_IRRF
                            EndIf
                            M->E2_IRRF := nCalcIr
                        EndIf

                    EndIf

                    IF lF050CIRF
                        M->E2_IRRF := ExecBlock("F050CIRF",.f.,.f.,nBaseIrrf)
                    Endif

                    // Verifica se Pessoa Fisica ou Juridica, para fins de
                    // calculo do irrf, considerando o calculo do CIDE.
                    IF __lLocBRA
                        IF (SA2->A2_TIPO == "F" .OR. SA2->A2_TIPO == "J") .AND. SED->ED_CALCCID == "S" .AND. SA2->A2_RECCIDE == "1"
                            m->e2_irrf := Round(((m->e2_valor * Iif(AllTrim(Str(m->e2_moeda,2)) $ "01", 1, If(M->E2_MOEDA > 1 .And. M->E2_TXMOEDA > 0, M->E2_TXMOEDA,;
                                        RecMoeda(m->e2_emissao,m->e2_moeda)))) * IIF(SED->ED_PERCIRF>0,SED->ED_PERCIRF,GetMV("MV_ALIQIRF"))/100)-nValIRRF,2)
                        Endif
                    EndIf
                EndIf
            EndIf
        EndIf
    Endif

    If FWisincallstack("FA050ISS") .and. lVlOnlyRet
        lVlOnlyRet := .F.
    Endif

    //Nao calculo impostos para alguns tipos de titulos
    If m->e2_tipo $ MVABATIM+"/"+MVPROVIS+"/"+MVTAXA+"/"+MVINSS+"/"+MVISS+"/"+MVTXA +"/"+"SES"+"/"+MV_CPNEG+"/"+"INA" .or. ;
          (AllTrim(M->E2_ORIGEM) $ "FINA290#FINA290M" .AND. (Alltrim(M->E2_FATURA) == "NOTFAT"))

        m->e2_pis := 0
        m->e2_cofins := 0
        m->e2_csll := 0
        nOldPis := 0
        nOldCofins := 0
        nOldCsll := 0

    ElseIf __lLocBRA
        //Caso não tenha o tratamento de base diferenciada para os impostos,
        //Verifica a utilizacao da base de impostos (imformada) ou o valor do titulo.
        If !lBaseImp .AND. M->E2_TIPO $ MVPAGANT
            If !__lPccMR
                If !Empty(M->E2_BASEPIS)
                    nBasePCC := M->E2_BASEPIS
                Else
                    nBasePCC := If(__nVlrMR > 0, __nVlrMR, M->E2_VALOR)
                Endif
            EndIf
        Else
            // Caso a funcao Fa050Nat2 tenha sido chamada a partir da alteracao dos campos de Irrf (.T.), Inss e Iss,
            // Fazemos os recalculos dos impostos da lei 10925  sem e fazer a recarga dos valores destes campos.
            If !lVlOnlyRet
                If !__lPccMR .And. M->E2_BASEPIS > 0 .AND. M->E2_BASEPIS <> M->E2_VALOR
                    nBasePCC := IIF ((M->E2_VALOR <= nVlMinImp),M->E2_VALOR,M->E2_BASEPIS)
                Else
                    If M->E2_VALOR <> SE2->E2_VALOR .And. lAltera .And. M->E2_VALOR <= nVlMinImp
                        If !__lPccMR .And. (lPccBaixa .Or. (!lPccBaixa .And. nBasePCC <> M->E2_BASEPIS))
                            nBasePCC := If(__nVlrMR > 0, __nVlrMR, SE2->E2_VALOR)
                        Endif
                    ElseIf !lF050Auto	// A base do pcc já foi obtida pela ExecAuto
                        nBasePCC := If(__nVlrMR > 0, __nVlrMR, m->e2_valor)
                    Endif
                Endif
            Else
                If __lBtrISS
                    nBasePCC := m->e2_valor + (xMoeda(m->e2_irrf + If(!lCalcIssBx,m->e2_iss + m->e2_btriss, 0) + m->e2_inss, 1, M->E2_MOEDA, M->E2_EMISSAO, 3, 1, nOldTxMoeda) )
                Else
                    nBasePCC := m->e2_valor + (xMoeda(m->e2_irrf + If(!lCalcIssBx,m->e2_iss, 0) + m->e2_inss, 1, M->E2_MOEDA, M->E2_EMISSAO, 3, 1, nOldTxMoeda) )
                EndIf
            Endif
        Endif

        If !__lIrfMR  .And. !lIRPFBaixa .And. M->E2_VALOR <> M->E2_BASEIRF
            If  ("M->E2_TXMOEDA" $ cField .And. lAltTxMoeda .And. !lAltValor ) .Or. "M->E2_BASEPIS" $ cField
                If !(lIRPFBaixa .And. M->E2_TIPO $ MVPAGANT)
                    If !lAplMinIR .Or.( lAplMinIR .And.(M->E2_IRRF > GetMv("MV_VLRETIR")))
                        If M->E2_MOEDA == 1
                            M->E2_VALOR += M->E2_IRRF
                        Else
                            M->E2_VALOR += xMoeda(M->E2_IRRF,1,M->E2_MOEDA,M->E2_EMISSAO,3,,nOldTxMoeda)
                        EndIf
                    Else
                        M->E2_VALOR += xMoeda(nIrrfAnt,1,M->E2_MOEDA,M->E2_EMISSAO,3,,nOldTxMoeda)
                        nIrrfAnt	:= 0
                    EndIf
                EndIf
            EndIf
        EndIf

        If !__lInsMR .And. M->E2_VALOR <> M->E2_BASEINS .And. cField $ "M->E2_TXMOEDA" .And. M->E2_MOEDA > 1 .And. M->E2_TXMOEDA > 1
            If  lAltTxMoeda .And. !lAltValor
                If M->E2_MOEDA == 1
                    M->E2_VALOR += M->E2_INSS
                Else
                    M->E2_VALOR += xMoeda(M->E2_INSS,1,M->E2_MOEDA,M->E2_EMISSAO,3,,nOldTxMoeda)
                EndIf
            EndIf
        Endif

        If !__lIssMR  .And. !lCalcIssBx .And. M->E2_VALOR <> M->E2_BASEPIS .And. cField $ "M->E2_BASEPIS/M->E2_TXMOEDA"
            If M->E2_MOEDA == 1
                M->E2_VALOR += M->E2_ISS + If(__lBtrISS,M->E2_BTRISS,0)
            Else
                M->E2_VALOR += (xMoeda(M->E2_ISS,1,M->E2_MOEDA,M->E2_EMISSAO,3,,M->E2_TXMOEDA) ) + If(__lBtrISS,xMoeda(M->E2_BTRISS,1,M->E2_MOEDA,M->E2_EMISSAO,3,,M->E2_TXMOEDA),0)
            EndIf
        EndIf

        //CÁLCULO DO PCC
        If !__lPccMR
	        If M->E2_EMISSAO < __dLastPCC .Or. lEmpPub //Se estiver configurado para empresa pública ou se for uma emissão anterior a data em que a lei 13.137 entrou em vigor (22/06/2015)

                //PIS
                If !lF050Auto .Or. !F050ImpAut("E2_PIS")
                    //Se a natureza pede cálculo do PIS e o fornecedor não recolhe
                    If SED->ED_CALCPIS == "S"  .And. SA2->A2_RECPIS == "2"
                       	nValPIS := nBasePCC * (SED->ED_PERCPIS / 100)

                        If ! GetNewPar("MV_RNDPIS",.F.)
                            nValPIS := NoRound( nValPIS, 2 )
                        Else
                        	nValPIS := Round( nValPIS, 2 )
                        Endif
                    Else
                        nValPIS := 0
                        nOldPis := 0
                    EndIf

                    M->E2_PIS := nValPIS
                    nPisCalc := nValPIS
                    nPisBaseC := nBasePCC
                EndIf

                //COFINS
                If !lF050Auto .Or. !F050ImpAut("E2_COFINS")
                    //Se a natureza pede cálculo do COFINS e o fornecedor não recolhe
                    If SED->ED_CALCCOF == "S" .And. SA2->A2_RECCOFI == "2"
                        nValCOF := nBasePCC * (SED->ED_PERCCOF / 100)

                        If ! GetNewPar("MV_RNDCOF",.F.)
                        	nValCOF := NoRound( nValCOF, 2 )
                        Else
                            nValCOF := Round( nValCOF, 2 )
                        Endif
                    Else
                       	nValCOF := 0
                        nOldCofins := 0
                    EndIf

                    M->E2_COFINS := nValCOF
                    nCofCalc := nValCOF
                    nCofBaseC := nBasePCC
                EndIF

                //CSLL
                If !lF050Auto .Or. !F050ImpAut("E2_CSLL")
                    //Se a natureza pede cálculo do CSLL  e o fornecedor não recolhe
                    If SED->ED_CALCCSL == "S"  .And. SA2->A2_RECCSLL == "2"
                        nValCSL := nBasePCC * (SED->ED_PERCCSL / 100)

                        If ! GetNewPar("MV_RNDCSL",.F.)
                            nValCSL := NoRound( nValCSL, 2 )
                        Else
                            nValCSL := Round( nValCSL, 2 )
                        Endif
                    Else
                        nValCSL := 0
                        nOldCsll := 0
                    Endif

                    M->E2_CSLL := nValCSL
                    nCslCalc := nValCSL
                    nCslBaseC := nBasePCC
                EndIf

                If !lPccBaixa                
                    If  lEmpPub 
                        FVerMinImp(nBasePcc,,lIrfRetAnt)
                        If lAplMinP .And. M->E2_PIS+M->E2_COFINS+M->E2_CSLL+M->E2_IRRF < nVlMPub .And. !lIrfRetAnt 			                
                            nPis := nCoFins := nCsll := nIrrf := 0
                            M->E2_PIS := M->E2_COFINS := M->E2_CSLL := M->E2_IRRF := 0                               
                            nVlRetPis := nVlRetCof := nVlRetCsl := 0  
                            aDadosRet[2] := aDadosRet[3] := aDadosRet[4] := 0
                        Endif
                    ElseIf !lEmpPub .And. M->E2_APLVLMN <> "2" //Caso não seja pela baixa e se aplique a verificação de valor mínimo do PCC, verifica o valor dos títulos
                        If !lAltera .And. nBasePCC > nVlMinImp
                            aRecSE2 := FImpExcTit("SE2", SE2->E2_PREFIXO, SE2->E2_NUM, SE2->E2_PARCELA, SE2->E2_TIPO, SE2->E2_FORNECE, SE2->E2_LOJA)
                            If Len(aRecSE2) > 0
                                aAreaSE2 := SE2->( GetArea() )
                                For nX := 1 to Len(aRecSE2)
                                    SE2->( MSGoto( aRecSE2[nX] ) )
                                    M->E2_PIS    += SE2->E2_PIS
                                    M->E2_COFINS += SE2->E2_COFINS
                                    M->E2_CSLL   += SE2->E2_CSLL
                                Next nX
                                RestArea(aAreaSE2)
                                FwFreeArray(aAreaSE2)
                            EndIf
                        EndIf
                        FVerMinImp(nBasePcc)
                    EndIf    
                ElseIf M->E2_TIPO $ MVPAGANT //Verifico se eh PA para calcular tx's na emissao
                	 nValPgto := nBasePCC

		            //Atualiza o valor do titulo antes de calcular as retencoes pendentes.
		            nVlAltSEST 	:= m->e2_sest
		            M->E2_VALOR := M->E2_VALOR - Round(xMoeda( Iif(!lCalcIssBx, M->E2_ISS + Iif(__lBtrISS, M->E2_BTRISS, 0), 0) +;
		                           Iif(SED->ED_RINSSPA == "1", M->E2_INSS, 0) + nValSEST,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA), 2)

	                //Grava campos da memoria (SE2), com conteudo das variaveis privates carregadas pela F080TotMes().
	                //Efetua varredura no SE5 para buscar titulos que ainda estejam pendentes retencao por valor insuficiente.
	                F080TotMes(M->E2_EMISSAO,.T.,.T.)

	                M->E2_PIS     := nPis
	                M->E2_COFINS  := nCofins
	                M->E2_CSLL    := nCsll
	                M->E2_VRETPIS := nVlRetPis
	                M->E2_VRETCOF := nVlRetCof
	                M->E2_VRETCSL := nVlRetCsl
                EndIf

	        Else //Emissão a partir data em que a lei 13.137 entrou em vigor (22/06/2015)

                If nVencto == 2
                    dRef := M->E2_VENCREA
                ElseIf nVencto == 1 .OR. EMPTY(nVencto)
                    dRef := M->E2_EMISSAO
                ElseIf nVencto == 3
                    dRef := IIf(Type("dDataEmis1") # "U", IIf(!Empty(dDataEmis1),dDataEmis1,dDataBase),dDataBase)
                Endif

                aPCC := newMinPcc(dRef, xMoeda(M->E2_BASEPIS,M->E2_MOEDA,1,M->E2_EMISSAO,3,M->E2_TXMOEDA), M->E2_NATUREZ, "P", M->(E2_FORNECE + E2_LOJA))

                If !lF050Auto .Or. !F050ImpAut("E2_PIS")
                    nPis := aPCC[2]
                    M->E2_PIS := nPis
                    nOldPis := nPis
                EndIf

                If !lF050Auto .Or. !F050ImpAut("E2_COFINS")
                    nCofins := aPCC[3]
                    M->E2_COFINS := nCofins
                    nOldCofins := nCofins
                Endif

                If !lF050Auto .Or. !F050ImpAut("E2_CSLL")
                    nCsll := aPCC[4]
                    M->E2_CSLL := nCsll
                    nOldCsll := nCsll
                EndIf

                If len(aPCC) > 4
                    __aTitCalc := aPCC[5]
                Endif

                If M->E2_TIPO $ MVPAGANT .Or. !lPccBaixa
                    nVlRetPis := nPis
                    nVlRetCof := nCofins
                    nVlRetCsl := nCsll
                Else
                    nVlRetPis := 0
                    nVlRetCof := 0
                    nVlRetCsl := 0
                EndIf

                M->E2_VRETPIS := nVlRetPis
                M->E2_VRETCOF := nVlRetCof
                M->E2_VRETCSL := nVlRetCsl

	        EndIf

        EndIf

    EndIf

    nVlAltInss 	:= If(!__lInsMR , m->e2_inss , 0)
    nVlAltSEST 	:= m->e2_sest

    lFirstAlt	:= .F.

    // Caso a funcao Fa050Nat2 tenha sido chamada a partir da alteracao dos campos de Irrf (.T.), Inss e Iss,
    // Fazemos os recalculos dos impostos da lei 10925 sem e fazer a recarga dos valores destes campos.
    If !lVlOnlyRet .and. ( !lAltVcto .Or. lF050Auto) .And. __lLocBRA
        // Se existir os campos de impostos a pagar, PIS, COFINS, CSLL - MP 135
        If !lPCCBaixa
            If !__lIssMR .and. !lCalcIssBx
                If __lBtrISS
                    M->E2_VALOR := M->E2_VALOR - Iif(M->E2_MOEDA == 1, m->e2_iss + m->e2_btriss, xMoeda(m->e2_iss + m->e2_btriss,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA))
                Else
                    M->E2_VALOR := M->E2_VALOR - Iif(M->E2_MOEDA == 1, m->e2_iss, xMoeda(m->e2_iss,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA) )
                EndIf
            EndIf

            If !__lInsMR
                M->E2_VALOR := M->E2_VALOR - Iif(M->E2_MOEDA == 1, m->e2_inss, xMoeda(m->e2_inss,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA) )
            EndIf

            If !__lSestMR
                M->E2_VALOR := M->E2_VALOR - Iif(M->E2_MOEDA == 1, nValSEST, xMoeda(nValSEST,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA) )
            EndIf

            If !__lPccMR
                If lAltera
                    If (SE2->E2_VALOR <>  M->E2_VALOR)
                        If !(M->E2_VALOR == M->E2_BASEPIS - (Iif(M->E2_MOEDA == 1,(m->e2_pis + m->e2_cofins + m->e2_csll), xMoeda((m->e2_pis + m->e2_cofins + m->e2_csll),1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA) )))
                            m->e2_valor -= Iif(M->E2_MOEDA == 1,(m->e2_pis + m->e2_cofins + m->e2_csll), xMoeda((m->e2_pis + m->e2_cofins + m->e2_csll),1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA) )
                        Endif
                    Elseif M->E2_VALOR == M->E2_BASEPIS
                        If !(M->E2_VALOR == M->E2_BASEPIS - (Iif(M->E2_MOEDA == 1,(m->e2_pis + m->e2_cofins + m->e2_csll), xMoeda((m->e2_pis + m->e2_cofins + m->e2_csll),1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA) )))
                            m->e2_valor -= Iif(M->E2_MOEDA == 1,(m->e2_pis + m->e2_cofins + m->e2_csll), xMoeda((m->e2_pis + m->e2_cofins + m->e2_csll),1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA) )
                        Endif
                        If M->E2_SALDO == M->E2_BASEPIS
                            m->e2_saldo -= Iif(M->E2_MOEDA == 1,(m->e2_pis + m->e2_cofins + m->e2_csll), xMoeda((m->e2_pis + m->e2_cofins + m->e2_csll),1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA) )
                        Endif
                    Endif
                Else
                    If M->E2_MOEDA == 1
                        M->E2_VALOR -= (M->E2_PIS+M->E2_COFINS+M->E2_CSLL)
                    Else
                        M->E2_VALOR := M->E2_VALOR - Round(xMoeda(M->E2_PIS,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA), 2)
                        M->E2_VALOR := M->E2_VALOR - Round(xMoeda(M->E2_COFINS,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA), 2)
                        M->E2_VALOR := M->E2_VALOR - Round(xMoeda(M->E2_CSLL,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA), 2)
                    EndIf
                Endif
            Endif

            If !lJaDescIr
                If !__lIrfMR .And. !lIRPFBaixa
                    If M->E2_MOEDA == 1
                        M->E2_VALOR -= m->e2_irrf
                    ElseIf cField == "M->E2_MOEDA" .And. M->E2_MOEDA > 1 .And. lSumIR .And. nCalcIr > 0
                        M->E2_VALOR := M->E2_VALOR - xMoeda(If(Empty(m->e2_irrf), nCalcIr, m->e2_irrf), 1, M->E2_MOEDA, M->E2_EMISSAO, 3,1, M->E2_TXMOEDA)
                    Else
                        M->E2_VALOR := M->E2_VALOR - IIF(nValIrOld > 0, nValIrOld, xMoeda(m->e2_irrf,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA))
                    EndIf
                EndIf
            EndIf

            If !__lIssMR .And. M->E2_TIPO $ MVPAGANT .And. lCalcIssBx //Caso ISS seja na emissao nesta ponto da rotina ele jah foi descontado.
                M->E2_VALOR := M->E2_VALOR - Iif(M->E2_MOEDA == 1,M->E2_ISS, xMoeda(M->E2_ISS,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA) )
            Endif

            If !__lIrfMR .And. M->E2_TIPO $ MVPAGANT .And. lIRPFBaixa .And. !lPaBruto //Caso IR seja na emissao nesta ponto da rotina ele jah foi descontado.
                M->E2_VALOR := M->E2_VALOR - Iif(M->E2_MOEDA == 1,M->E2_IRRF, xMoeda(M->E2_IRRF, 1, M->E2_MOEDA, M->E2_EMISSAO, 3, 1, M->E2_TXMOEDA) )
            Endif

            If lPrImPA
                If !__lInsMR
                    m->e2_valor := m->e2_valor - Iif(M->E2_MOEDA == 1,M->E2_PRINSS, xMoeda(M->E2_PRINSS,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA) )
                EndIf
                If !__lIssMR .And. !lCalcIssBx
                    M->E2_VALOR := M->E2_VALOR - Iif(M->E2_MOEDA == 1,M->E2_PRISS, xMoeda(M->E2_PRISS,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA) )
                EndIf
            EndIf
        Else
            If M->E2_TIPO $ MVPAGANT .And. !lInsPub
                If !(lEmpPub .And. lIRPFBaixa .And. lAplMinP .And. M->(E2_PIS+E2_COFINS+E2_CSLL+E2_IRRF) < nVlMPub)
                    If !__lPccMR
                        If M->E2_MOEDA == 1
                            M->E2_VALOR -= (M->E2_PIS+M->E2_COFINS+M->E2_CSLL)
                        Else
                            M->E2_VALOR := M->E2_VALOR - Round(xMoeda(M->E2_PIS,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA), 2)
                            M->E2_VALOR := M->E2_VALOR - Round(xMoeda(M->E2_COFINS,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA), 2)
                            M->E2_VALOR := M->E2_VALOR - Round(xMoeda(M->E2_CSLL,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA), 2)
                        EndIf
                    Endif
                EndIf

                lDescIr := .F.

                If lEmpPub
                    If lIRPFBaixa .And. lAplMinP .And. M->(E2_PIS+E2_COFINS+E2_CSLL+E2_IRRF) >= nVlMPub .and. !lPaBruto
                        lDescIr := .T. 
                    Endif    
                Else
                    If !__lIrfMR .And. M->E2_TIPO $ MVPAGANT .And. !lPaBruto  
                        lDescIr := .T.
                    Endif
                Endif    
                If lDescIr
                	M->E2_VALOR := M->E2_VALOR - Iif(M->E2_MOEDA == 1,M->E2_IRRF, xMoeda(M->E2_IRRF, 1, M->E2_MOEDA, M->E2_EMISSAO, 3, 1, M->E2_TXMOEDA) )
                Endif
                If lPrImPA .and. !lPaBruto
                    If !__lInsMR
                        m->e2_valor := m->e2_valor - Iif(M->E2_MOEDA == 1,M->E2_PRINSS, xMoeda(M->E2_PRINSS,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA) )
                    EndIf
                    If !__lIssMR .And. !lCalcIssBx
                        M->E2_VALOR := M->E2_VALOR - Iif(M->E2_MOEDA == 1, M->E2_PRISS, Round(xMoeda(M->E2_PRISS,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA),2))
                    EndIf
                EndIf

                If !__lIssMR .And. ((!lCalcIssBx .And. lIRPFBaixa) .Or. lCalcIssBx)//Casos em que o ISS deve ser descontado na variavel E2_VALOR.
                    If __lBtrISS
                        M->E2_VALOR := M->E2_VALOR - ROUND(xMoeda(m->e2_iss + m->e2_btriss,1,M->E2_MOEDA,M->E2_EMISSAO,MsDecimais(M->E2_MOEDA)+1,,M->E2_TXMOEDA),MsDecimais(M->E2_MOEDA))
                    Else
                        M->E2_VALOR := M->E2_VALOR - ROUND(xMoeda(m->e2_iss,1,M->E2_MOEDA,M->E2_EMISSAO,MsDecimais(M->E2_MOEDA)+1,,M->E2_TXMOEDA),MsDecimais(M->E2_MOEDA))
                    EndIf
                Endif

                If !__lInsMR
                	M->E2_VALOR := M->E2_VALOR - Iif(M->E2_MOEDA == 1, m->e2_inss, xMoeda(m->e2_inss,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA) )
                EndIf

            Else
                If cPaisLoc== "BRA"

                    If __nImpMR == 0 .And. M->E2_MOEDA == 1
                        M->E2_VALOR := nValDig
                    Endif

                    If !__lIssMR .and. !lCalcIssBx
                        If __lBtrISS
                            M->E2_VALOR := M->E2_VALOR - ROUND(xMoeda(m->e2_iss + m->e2_btriss,1,M->E2_MOEDA,M->E2_EMISSAO,MsDecimais(M->E2_MOEDA)+1,,M->E2_TXMOEDA),MsDecimais(M->E2_MOEDA))
                        Else
                            M->E2_VALOR := M->E2_VALOR - ROUND(xMoeda(m->e2_iss,1,M->E2_MOEDA,M->E2_EMISSAO,MsDecimais(M->E2_MOEDA)+1,,M->E2_TXMOEDA),MsDecimais(M->E2_MOEDA))
                        EndIf

                        If lPrImPA
                            M->E2_VALOR := M->E2_VALOR - Iif(M->E2_MOEDA == 1,M->E2_PRISS, xMoeda(m->E2_PRISS,1,M->E2_MOEDA,M->E2_EMISSAO,MsDecimais(M->E2_MOEDA)+1,,M->E2_TXMOEDA))
                        Endif
                    Endif

                    If !__lInsMR
                        M->E2_VALOR := M->E2_VALOR - ROUND(xMoeda(m->e2_inss,1,M->E2_MOEDA,M->E2_EMISSAO,MsDecimais(M->E2_MOEDA)+1,,M->E2_TXMOEDA),MsDecimais(M->E2_MOEDA))
                    Endif

                    If !__lIrfMR .and. !lIRPFBaixa .And. !lJaDescIr
                        M->E2_VALOR := M->E2_VALOR - xMoeda(m->e2_irrf,1,M->E2_MOEDA,M->E2_EMISSAO,MsDecimais(M->E2_MOEDA)+1,,M->E2_TXMOEDA)
                    EndIf

                    If !__lSestMR
                        M->E2_VALOR := M->E2_VALOR - xMoeda(nValSEST,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA)
                    Endif

                    If cPaisLoc <> "BRA" .And. ReadVar() == "M->E2_VALOR" .And. M->E2_VALOR == 0
                        If nValBruto > 0
                            M->E2_VALOR := nValBruto
                        EndIf
                    EndIf

                    If !__lInsMR
                        If lPrImPA
                            m->e2_valor := m->e2_valor - Iif(M->E2_MOEDA == 1,M->E2_PRINSS, xMoeda(M->E2_PRINSS,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA) )
                            If !lCalcIssBx
                                m->e2_valor := m->e2_valor - M->E2_PRISS
                            EndIf
                        Endif
                    EndIf
                EndIf
            Endif
        Endif
        //Restitui os impostos para PA BRUTO
        If (m->e2_tipo $ MVPAGANT .and. lPaBruto) .and. (!lInsPub .or. (lInsPub .and. ReadVar() <> "M->E2_VALOR"))
            If !__lPccMR
                m->e2_valor := m->e2_valor + Iif(M->E2_MOEDA == 1,(m->e2_pis + m->e2_cofins + m->e2_csll), xMoeda((m->e2_pis + m->e2_cofins + m->e2_csll),1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA) )
            EndIf
            If !__lIrfMR
                M->E2_VALOR := M->E2_VALOR + If(M->E2_MOEDA == 1,0, xMoeda(m->e2_irrf,1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA) )
            EndIf
            If !__lIssMR
                M->E2_VALOR := M->E2_VALOR + If(M->E2_MOEDA == 1,If(__lBtrISS,m->e2_iss+m->e2_btriss,m->e2_iss), xMoeda(If(__lBtrISS,m->e2_iss+m->e2_btriss,m->e2_iss),1,M->E2_MOEDA,M->E2_EMISSAO,3,1,M->E2_TXMOEDA) )
            EndIf
        Endif

        If !__lIrfMR .And. m->e2_valor < 0
            m->e2_irrf  += m->e2_valor - 0.01
            m->e2_valor := 0.01
        EndIf

        //Verifica se havera retencao do INSS
        If !__lInsMR .And. SED->ED_DEDINSS == "2"  //Nao desconta o Inss do principal
            M->E2_VALOR := M->E2_VALOR + ROUND(xMoeda(m->e2_inss,1,M->E2_MOEDA,M->E2_EMISSAO,MsDecimais(M->E2_MOEDA)+1,,M->E2_TXMOEDA),MsDecimais(M->E2_MOEDA))
        Endif

    Endif
    // Calculo das Retenções - Republica Dominicana
    If 	cPaisLoc $ "DOM|COS"
        Help(" ",1,"FA050FRM",,"Tabela 'FRM', 'FRN' ou 'CCR' não faz parte do Dicionário de Dados") //"Tabela 'FRM', 'FRN' ou 'CCR' não faz parte do Dicionário de Dados"
    EndIf

    // Inicializa o valor em Real como sugestao
    M->E2_VLCRUZ := Round(NoRound(xMoeda(M->E2_VALOR,M->E2_MOEDA,1,M->E2_EMISSAO,MsDecimais(1)+1,M->E2_TXMOEDA,1),MsDecimais(1)+1),MsDecimais(1))

    m->e2_saldo := m->e2_valor
    nOldValor	:= m->e2_valor
    nOldSaldo 	:= m->e2_saldo
    nOldIRR		:= m->e2_irrf
    nOldISS		:= m->e2_iss
    If __lBtrISS
        nOldBtrISS	:= m->e2_btriss
    EndIf
    nOldInss	:= m->e2_inss
    nOldSEST	:= m->e2_sest
    nOldPis		:= m->e2_pis
    nOldCofins	:= m->e2_cofins
    nOldCsll	:= m->e2_csll
    nOldValorPg := nOldValor
    lRefresh 	:= .T.

Return .t.


//-------------------------------------------------------
/*/{Protheus.doc} FA050AxInc

Função para complementacao da inclusao de C.Pagar

@author Mauricio Pequim Jr.
@since 04/08/99
@version P12
/*/
//-------------------------------------------------------
Function FA050AxInc(cAlias AS Character)

    Local lRet          AS Logical
    Local nSavRec       AS Numeric
    Local nSavRecA2     AS Numeric
    Local cArquivo      AS Character
    Local cPadrao       AS Character
    Local lPadrao	    AS Logical
    Local nTotal	    AS Numeric
    Local nHdlPrv	    AS Numeric
    Local nIndex 	    AS Numeric
    Local nValSaldo	    AS Numeric
    Local lHeader	    AS Logical
    Local lDesdobr 	    AS Logical
    Local nMoedSE2 	    AS Numeric
    Local cSeq          AS Character
    Local lF050Inc      AS Logical
    Local cOrdPago      AS Character
    Local cBancoCx      AS Character
    Local nRecCtb	    AS Numeric
    Local aParc 	    AS Array
    Local aTps		    AS Array
    Local nX 		    AS Numeric
    Local aRecnos 	    AS Array
    Local nLoop 	    AS Numeric
    Local nSobra 	    AS Numeric
    Local nValorTit     AS Numeric
    Local nRetOriPIS    AS Numeric
    Local nRetOriCOF    AS Numeric
    Local nRetOriCSL    AS Numeric
    Local nVlMinImp     AS Numeric
    Local nFatorRed     AS Numeric
    Local lRetParc	    AS Logical
    Local lRestValImp   AS Logical
    Local nInss 	    AS Numeric
    Local cPrefOri      AS Character
    Local cNumOri       AS Character
    Local cParcOri      AS Character
    Local cTipoOri      AS Character
    Local cCfOri        AS Character
    Local cLojaOri      AS Character
    Local nDiferImp     AS Numeric
    Local lDigitado     AS Logical
    Local lPccBxPA      AS Logical
    Local lFAPodeTVA    AS Logical
    Local lPCCBaixa     AS Logical
    Local cNccRet		AS Character
    Local lIRPFBaixa 	AS Logical
    Local nCalcPis 		AS Numeric
    Local nCalcCof 		AS Numeric
    Local nCalcCsl 		AS Numeric
    Local nVlPrinc      AS Numeric
    Local lCalcIssBx    AS Logical
    Local lSetAuto		AS Logical
    Local lSetHelp		AS Logical
    Local cProcPCO		AS Character
    Local cItemPCO		AS Character
    Local cRecPag 		AS Character
    Local aFlagCTB 		AS Array
    Local lUsaFlag		AS Logical
    Local lCtMovPa		AS Logical
    Local nRecSE2		AS Numeric
    Local lSisAltPIS	AS Logical
    Local lSisAltCOF	AS Logical
    Local lSisAltCSL 	AS Logical
    Local aDiario	 	AS Array
    Local lEnd			AS Logical
    Local lAtuSldNat 	AS Logical
    Local lEmpPub    	AS Logical
    Local lAplMinP      AS Logical
    Local lCopy  		AS Logical
    Local lEmprest  	AS Logical
    Local lMile         AS Logical
    Local cFilAux       AS Character
    //Base de imposto Variavel
    Local lBaseImp	 	AS Logical
    Local lFA050CT		AS Logical
    // Ignora recalculo de impostos
    Local lRefImp		AS Logical
    Local lTmsOper		AS Logical
    //Controle de Desdobramento
    Local oMBrowse 		AS Object
    Local lSpbInUse 	AS Logical
    Local cModSpb		AS Character
    //Nova estrutura SE5
    Local oModel        AS Object
    Local oSubFKA       AS Object
    Local cLog          AS Character
    Local aAreaAnt      AS Array
    Local lFA050GRV     AS Logical
    Local lFA050FIN     AS Logical
    //Motor de Retenções.
    Local aMotRet       AS Array
    Local cPret         AS Character
    Local nTcSql        AS Numeric
    Local lF986Imp      AS Logical

    Default __lNRasDSD := SuperGetMV("MV_NRASDSD",.T.,.F.)
    Default __lIntPFS  := SuperGetMv("MV_JURXFIN",.T.,.F.) //Integração do Financeiro com o Juridico(Habilitado = .T.)

    lRet            := .T.
    lPadrao	        := .F.
    nTotal	        := 0
    nHdlPrv	        := 0
    nIndex 	        := IndexOrd()
    nValSaldo	    := 0
    lHeader	        := .F.
    lDesdobr 	    := .F.
    nMoedSE2 	    := SE2->E2_MOEDA
    lF050Inc 	    := (ExistBlock("F050INC"))
    nRecCtb	        := 0
    aParc 	        := {}
    aTps		    := {}
    nX 		        := 0
    aRecnos 	    := {}
    nLoop 	        := 0
    nSobra 	        := 0
    nValorTit       := 0
    nRetOriPIS      := 0
    nRetOriCOF      := 0
    nRetOriCSL      := 0
    nVlMinImp       := GetNewPar("MV_VL10925",5000)
    nFatorRed       := 0
    lRetParc	    := .T.
    lRestValImp     := .F.
    nInss 	        := SE2->E2_INSS
    cPrefOri        := SE2->E2_PREFIXO
    cNumOri         := SE2->E2_NUM
    cParcOri        := SE2->E2_PARCELA
    cTipoOri        := SE2->E2_TIPO
    cCfOri          := SE2->E2_FORNECE
    cLojaOri        := SE2->E2_LOJA
    nDiferImp       := 0
    lDigitado       := .F.
    lPccBxPA        := .F.
    lFAPodeTVA      := ExistFunc("FAPodeTVA")
    //Controla o Pis Cofins e Csll na baixa
    lPCCBaixa       := SuperGetMv("MV_BX10925",.T.,"2") == "1"
    //1-Cria NCC/NDF referente a diferenca de impostos entre emitidos (SE2) e retidos (SE5)
    //2-Nao Cria NCC/NDF, ou seja, controla a diferenca num proximo titulo
    //3-Nao Controla
    cNccRet		    := SuperGetMv("MV_NCCRET",.F.,"1")
    lIRPFBaixa 	    := .F.
    nCalcPis 		:= 0
    nCalcCof 		:= 0
    nCalcCsl 		:= 0
    nVlPrinc        := 0
    lCalcIssBx      := IsIssBx("P")
    lSetAuto		:= .F.
    lSetHelp		:= .F.
    cProcPCO		:= "000021"
    cItemPCO		:= "01"
    cRecPag 		:= "P"
    aFlagCTB 		:= {}
    lUsaFlag		:= SuperGetMV( "MV_CTBFLAG" , .T. /*lHelp*/, .F. /*cPadrao*/)
    lCtMovPa		:= SuperGetMv("MV_CTMOVPA",.T.,"1") == "2" // Indica se a Contabilizacao do LP513 ocorrer pelo Titulo(SE2) ou Mov.Bancario(SE5) do Pagamento Antecipado. 1="SE2" / 2="SE5"
    nRecSE2		    := 0
    lSisAltPIS	    := .F.
    lSisAltCOF	    := .F.
    lSisAltCSL 	    := .F.
    aDiario	 	    := {}
    lEnd			:= .F.
    lAtuSldNat 	    := .T.
    lEmpPub    	    := IsEmpPub()
    lAplMinP        := .F.
    lCopy  		    := FwIsInCallStack("FINA631")
    lEmprest  	    := FwIsInCallStack("FINA171") .and. (AllTrim(SE2->E2_PREFIXO) == "EMP" .and. AllTrim(SE2->E2_TIPO) == "PR" .and. AllTrim(SE2->E2_NATUREZ) == "EMPRESTIMO")
    //Importacao via MILE
    lMile           = FwIsInCallStack("CFG600LMdl") .Or. FwIsInCallStack("FWMILEIMPORT") .Or. FwIsInCallStack("FWMILEEXPORT")
    cFilAux         = ""
    //Base de imposto Variavel
    lBaseImp	 	:= F050BSIMP(2)	//Verifica a existência dos campos
    lFA050CT		:= Existblock("FA050CT")
    // Ignora recalculo de impostos
    lRefImp		    := SuperGetMv('MV_REFIMP',,.F.)    //-- Usado pelo TMS com Operadora de Frota
    lTmsOper		:= SuperGetMv('MV_VSREPOM',,'1')  == '2' .And. SuperGetMv('MV_TMSOPDG',,'1')  == '2'
    //Controle de Desdobramento
    oMBrowse 		:= GetObjBrow()
    lSpbInUse 	    := SpbInUse()
    cModSpb		    := "1"
    //Nova estrutura SE5
    oModel          := NIL
    oSubFKA         := NIL
    cLog            := ""
    aAreaAnt        := {}
    lFA050GRV       := ExistBlock("FA050GRV")
    lFA050FIN       := ExistBlock("FA050FIN")
    //Motor de Retenções.
    aMotRet         := AClone(__aVetImp)
    cPret           := ""
    lF986Imp        := .F.
    
    If FindFunction("F986RImp")
        lF986Imp        := F986RImp()
    EndIf 

    //restaura valor inicial daS staticas
    __aVetImp := {}
    __lRatDes := .F.

    If SE2->E2_EMISSAO >= __dLastPCC .and. !lEmpPub
        nVlMinImp := 0
    EndIf

    If lSpbInUse
        cModSpb := IIf(Empty(SE2->E2_MODSPB), "1",SE2->E2_MODSPB)
    Endif
    If lRefImp .And. lTmsOper .And. (FwIsInCallStack('TMSQUITAC') .Or. FwIsInCallStack('TMA250SE2'))
        lBaseImp := .F.
    EndIf

    If Type("lAltValor") <> "L"
        lAltValor := .F.
    ElseIf !lAltValor
        lAltValor := (Type("nOldValorPg") == "N" .And. STR(nOldValorPg,17,2) != STR(M->E2_VALOR,17,2))
    Endif

    If !__lInsMR
        IF SED->ED_DEDINSS == "2"  //Nao desconta o INSS do principal
            nInss := 0
        Endif
    Endif

    If Type("aColsSev") != "A"
        aColsSev := {}
    Endif
    If Type("aColsSev") != "A"
        aHeaderSev := {}
    Endif

    If __lGesplan == Nil
        __lGesplan  := SuperGetMv("MV_FINTGES",.F.,.F.) .And. FindFunction("FUpdStamp")
    EndIF

    cBancoCx:=SuperGetMv("MV_CARTEIR",,"")

    If M->E2_TIPO == MVPAGANT
        IF !(E2_ACRESC = 0 .and. E2_DECRESC = 0)
            help("", 1, "F050PAAD",, STR0320, 1, 0)  // "Títulos do tipo PA não podem ter valores de acréscimo ou decréscimo."
            lRet := .F.
        EndIf
    EndIf

    If lRet
        If lMile .And. Type("M->E2_FILORIG") # Nil .And. !Empty(M->E2_FILORIG)
            cFilAux := cFilAnt
            cFilAnt := M->E2_FILORIG
        EndIf

        dbSelectArea("SA2")
        DbSetOrder(1)
        dbSeek(xFilial("SA2")+SE2->E2_FORNECE+SE2->E2_LOJA)

        nSavRecA2 := RecNo()
        lIRProg := IIf(__lLocBRA,IIf(!Empty(SA2->A2_IRPROG),SA2->A2_IRPROG,"2"),"2")

        lIRPFBaixa := IIf( __lLocBRA, SA2->A2_CALCIRF == "2", .F.)

        If lPCCBaixa
            lEmpPub := (lEmpPub .and. lIRPFBaixa)
        EndIf    
        If SA2->A2_MINPUB == "2"
            lAplMinP := .T.
        EndIf
        dbSelectArea(cAlias)
        RecLock(cAlias)
        
        // Grava filial do titulo com base no arq txt
        If lMile .And. Type("M->E2_FILIAL") # Nil
            SE2->E2_FILIAL := xFilial("SE2", cFilAnt)
        EndIf

        SE2->E2_NOMFOR	 := SA2->A2_NREDUZ
        SE2->E2_EMIS1	 := IIf(Type("dDataEmis1") # "U", IIf(!Empty(dDataEmis1),dDataEmis1,dDataBase),dDataBase)
        SE2->E2_VENCORI  := E2_VENCTO
        SE2->E2_SALDO	 := E2_VALOR
		If AllTrim(SE2->E2_ORIGEM) $ "SIGATMS"
			SE2->E2_VLCRUZ := Round(xMoeda(M->E2_VALOR,M->E2_MOEDA,1,M->E2_EMISSAO,TamSx3("E2_TXMOEDA")[2],M->E2_TXMOEDA),2)
		EndIf
        SE2->E2_BAIXA	 := CtoD("  /  /  ")
        SE2->E2_NUMBCO   := cChequeAdt
        SE2->E2_ORIGEM   := If(Empty(SE2->E2_ORIGEM),"FINA050",SE2->E2_ORIGEM)
        SE2->E2_LA		 := Iif(cPaisLoc $ "ARG|ANG|COL|MEX" .And. Alltrim(SE2->E2_TIPO) == "PA","S",Iif (lCopy .and. !Empty(M->E2_LA),"S",Iif(Type("lF050Auto") == "L" .And. lF050Auto .and. M->E2_LA == "S","S"," " )))
        SE2->E2_SDACRES  := E2_ACRESC
        SE2->E2_SDDECRE  := E2_DECRESC
        IF cPaisLoc=="EUA" .and. SE2->(FieldPos("E2_SLPLAID")) > 0//Para uso de integracion con PLAID
            SE2->E2_SLPLAID	 := SE2->E2_VALOR
            SE2->E2_BCOPAG	 := SA2->A2_BANCO
        ENDIF

        If SE2->E2_TIPO $ MVABATIM // Grava titulo pai no abatimento
            SE2->E2_TITPAI := cTitPaiAB
        EndIf

        If !__lIssMR .And. lCalcIssBx
            SE2->E2_TRETISS := "2"
        Endif
        SE2->E2_FILORIG  := If(Empty(SE2->E2_FILORIG),cFilAnt,SE2->E2_FILORIG)

        //Final da gravacao do titulo principal
        FKCOMMIT()
        nSavRec	:= RecNo()

        If lF050Inc
            ExecBlock("F050INC",.F.,.F.)
        EndIf

        //Chamada de funcao para tratamento da Average
        If lIntegracao
            FI400VALFIN()
        EndIf
        // Rotina de complemento de grava‡„o de t¡tulo a pagar
        // ser  utilizada apenas se o titulo NŽO for desdobra-
        // bramento. Caso seja um desdobramento, somar  o valor
        // das parcelas par atualizar o saldo do fornecedor.
        nValSaldo := 0

        If ( __lLocBRA )
            If __lPccMR
                If M->E2_TIPO $ MVPAGANT .And. M->(E2_PIS+E2_COFINS+E2_CSLL) > 0
                    RecLock("SE2",.F.)
                    If M->E2_PIS > 0
                        SE2->E2_VRETPIS := SE2->E2_PIS
                    EndIf
                    If M->E2_COFINS > 0
                        SE2->E2_VRETCOF := SE2->E2_COFINS
                    EndIf
                    If M->E2_CSLL > 0
                        SE2->E2_VRETCSL := SE2->E2_CSLL
                    EndIf
                    MsUnlock()
                EndIf
            Else
                If M->E2_TIPO $ MVPAGANT+"/"+MV_CRNEG .And. lPccBaixa //Se for PA e for pela Baixa (pis, cofins e csll), verifica valores digitados manualmente
                    //PIS digitado manualmente
                    If M->E2_PIS > 0
                        RecLock("SE2",.F.)
                        nVlRetPis     := SE2->E2_PIS
                        SE2->E2_VRETPIS := nVlRetPis
                        MsUnlock()
                        lDigitado := .T.
                    EndIf
                    //COFINS digitado manualmente
                    If M->E2_COFINS > 0
                        RecLock("SE2",.F.)
                        nVlRetCof     := SE2->E2_COFINS
                        SE2->E2_VRETCOF := nVlRetCof
                        MsUnlock()
                        lDigitado := .T.
                    EndIf
                    //CSLL digitado manualmente
                    If M->E2_CSLL > 0
                        RecLock("SE2",.F.)
                        nVlRetCsl     := SE2->E2_CSLL
                        SE2->E2_VRETCSL := nVlRetCsl
                        MsUnlock()
                        lDigitado := .T.
                    EndIf
                Else
                    //PIS digitado manualmente
                    If (SED->ED_CALCPIS == "N" .OR. SA2->A2_RECPIS == "1" .OR. lAltValor) .and. M->E2_PIS > 0
                        nVlRetPis := M->E2_PIS
                        lDigitado := .T.
                    EndIf
                    //COFINS digitado manualmente
                    If (SED->ED_CALCCOF == "N" .OR. SA2->A2_RECCOFI == "1" .OR. lAltValor) .and. M->E2_COFINS > 0
                        nVlRetCof := M->E2_COFINS
                        lDigitado := .T.
                    EndIf
                    //CSLL digitado manualmente
                    If (SED->ED_CALCCSL == "N" .OR. SA2->A2_RECCSLL == "1" .OR. lAltValor) .and. M->E2_CSLL > 0
                        nVlRetCsl := M->E2_CSLL
                        lDigitado := .T.
                    EndIf
                Endif
            EndIf
        Else
            nVlRetPis := 0
            nVlRetCof := 0
            nVlRetCsl := 0
        EndIf

        //Se o titulo eh um PA forca a geracao dos tx's na emissao
        If SE2->E2_TIPO $ MVPAGANT
            lPccBxPA := .T.
        EndIf

        //Alteracao na posicao do tratamento de desdobramento, para que caso o usuario cancele o desdobramento, o titulo receba o
        //tratamento de um titulo sem desdobramento
        //FNC : 00000028610/2009
        If SE2->E2_DESDOBR == "S"
            lDesdobr := .T.
            __lRatDes  := .T.
            nRecSe2 := SE2->(RECNO())
            //realiza a gravacao do model do titulo desdobrado
            If __lLocBRA
                Fa986grava("SE2","FINA050")
            EndIf
            Processa({|| GeraParcSe2(cAlias,@lEnd,@nHdlPrv,@nTotal,@cArquivo,nSavRecA2,nSavRec,lUsaFlag,@aFlagCTB)})
            SE2->(DbGoTo(nRecSe2))
            lHeader := nHdlPrv > 0
            If lEnd
                lDesdobr := .F.
            EndIf
        EndIf

        // Atualiza dados complementares do titulo
        If SE2->E2_DESDOBR == "N"
            If !__lPccMR
                If !lDigitado .and. !lPccBaixa

                    //Controle de base de impostos
                    If lBaseImp .And. SE2->E2_BASEPIS > 0
                        nValorTit := SE2->E2_BASEPIS
                    Else
                        nValorTit := SE2->(E2_VALOR+E2_PIS+E2_COFINS+E2_CSLL+Iif(lIRPFBaixa,0,E2_IRRF)+ E2_INSS +E2_ISS)+;
                        Iif(__lLocBRA, SE2->E2_SEST, 0)
                    Endif

                    Do Case
                        Case cModRetPIS == "1"
                            If aDadosRet[ 1 ] + nValorTit	> nVlMinImp
                                lRetParc := .T.
                                // Guarda os valores originais
                                nRetOriPIS := nVlRetPis
                                nRetOriCOF := nVlRetCOF
                                nRetOriCSL := nVlRetCSL

                                If cNCCRet == "2" .And. aDadosImp[1] <> aDadosRet[2]
                                    nVlRetPis += aDadosImp[1]
                                EndIf

                                If cNCCRet == "2" .And. aDadosImp[2] <> aDadosRet[3]
                                    nVlRetCof += aDadosImp[2]
                                EndIf

                                If cNCCRet == "2" .And. aDadosImp[3] <> aDadosRet[4]
                                    nVlRetCsl += aDadosImp[3]
                                EndIf

                                nVlRetPIS := aDadosRet[ 2 ] + nVlRetPis
                                nVlRetCOF := aDadosRet[ 3 ] + nVlRetCOF
                                nVlRetCSL := aDadosRet[ 4 ] + nVlRetCSL

                                nTotARet := nVlRetPIS + nVlRetCOF + nVlRetCSL

                                nValorTit := SE2->(E2_VALOR+E2_PIS+E2_COFINS+E2_CSLL)

                                nSobra := nValorTit - nTotARet

                                If nSobra < 0

                                    nFatorRed := 1 - ( Abs( nSobra ) / nTotARet )

                                    nVlRetPIS  := NoRound( nVlRetPIS * nFatorRed, 2 )
                                    nVlRetCOF  := NoRound( nVlRetCOF * nFatorRed, 2 )

                                    nVlRetCSL := nValorTit - ( nVlRetPIS + nVlRetCOF ) - 0.01

                                    //Gero NCC com a diferenca
                                    nDiFerImp := nTotARet - (nVlRetPIS + nVlRetCOF + nVlRetCSL)
                                    If cNccRet == "1"
                                        ADupCredRt(nDiferImp,"501",SE2->E2_MOEDA,.T.)
                                    Endif

                                EndIf

                                lRestValImp := .T.

                                // Grava os novos valores de retencao para este registro
                                RecLock( "SE2", .F. )
                                SE2->E2_PIS		:= nVlRetPIS
                                SE2->E2_COFINS	:= nVlRetCOF
                                SE2->E2_CSLL	:= nVlRetCSL
                                MsUnlock()
                                nSavRec 		:= SE2->( Recno() )

                                // Exclui a Marca de "pendente recolhimento" dos demais registros
                                If aDadosRet[1] > 0
                                    aRecnos := aClone( aDadosRet[ 5 ] )

                                    cPrefOri  := SE2->E2_PREFIXO
                                    cNumOri   := SE2->E2_NUM
                                    cParcOri  := SE2->E2_PARCELA
                                    cTipoOri  := SE2->E2_TIPO
                                    cCfOri    := SE2->E2_FORNECE
                                    cLojaOri  := SE2->E2_LOJA

                                    For nLoop := 1 to Len( aRecnos )

                                        SE2->( dbGoto( aRecnos[ nLoop ] ) )

                                        RecLock("SE2", .F. )

                                        SE2->E2_PRETPIS := "2"
                                        SE2->E2_PRETCOF := "2"
                                        SE2->E2_PRETCSL := "2"

                                        SE2->( MsUnlock() )

                                        If nSavRec <> aRecnos[ nLoop ]
                                            dbSelectArea("SFQ")
                                            RecLock("SFQ",.T.)
                                            SFQ->FQ_FILIAL  := xFilial("SFQ")
                                            SFQ->FQ_ENTORI  := "SE2"
                                            SFQ->FQ_PREFORI := cPrefOri
                                            SFQ->FQ_NUMORI  := cNumOri
                                            SFQ->FQ_PARCORI := cParcOri
                                            SFQ->FQ_TIPOORI := cTipoOri
                                            SFQ->FQ_CFORI   := cCfOri
                                            SFQ->FQ_LOJAORI := cLojaOri

                                            SFQ->FQ_ENTDES  := "SE2"
                                            SFQ->FQ_PREFDES := SE2->E2_PREFIXO
                                            SFQ->FQ_NUMDES  := SE2->E2_NUM
                                            SFQ->FQ_PARCDES := SE2->E2_PARCELA
                                            SFQ->FQ_TIPODES := SE2->E2_TIPO
                                            SFQ->FQ_CFDES   := SE2->E2_FORNECE
                                            SFQ->FQ_LOJADES := SE2->E2_LOJA
                                            MsUnlock()
                                        Endif
                                    Next nLoop
                                EndIf
                                // Retorna do ponteiro do SE2 para a parcela
                                SE2->( MsGoto( nSavRec ) )
                                Reclock("SE2", .F. )

                            Else
                                // Grava a Marca de "pendente recolhimento" dos demais registros
                                nRetOriPIS := nVlRetPis
                                nRetOriCOF := nVlRetCOF
                                nRetOriCSL := nVlRetCSL
                                If nRetOriPIS + nRetOriCof + nRetOriCsl > 0
                                    Reclock("SE2", .F. )
                                    SE2->E2_PRETPIS := "1"
                                    SE2->E2_PRETCOF := "1"
                                    SE2->E2_PRETCSL := "1"
                                    SE2->( MsUnlock() )
                                EndIf
                                lRetParc := .F.
                                lRestValImp := .T.
                            EndIf
                            
                        Case cModRetPIS == "2"
                            // Efetua a retencao
                            nSavRec := SE2->( Recno() )

                            // Exclui a Marca de "pendente recolhimento" dos demais registros
                            If aDadosRet[1] > 0
                                aRecnos := aClone( aDadosRet[ 5 ] )

                                cPrefOri  := SE2->E2_PREFIXO
                                cNumOri   := SE2->E2_NUM
                                cParcOri  := SE2->E2_PARCELA
                                cTipoOri  := SE2->E2_TIPO
                                cCfOri    := SE2->E2_FORNECE
                                cLojaOri  := SE2->E2_LOJA

                                For nLoop := 1 to Len( aRecnos )

                                    SE2->( dbGoto( aRecnos[ nLoop ] ) )

                                    RecLock("SE2", .F. )

                                    SE2->E2_PRETPIS := "2"
                                    SE2->E2_PRETCOF := "2"
                                    SE2->E2_PRETCSL := "2"

                                    SE2->( MsUnlock() )

                                    If nSavRec <> aRecnos[ nLoop ]
                                        dbSelectArea("SFQ")
                                        RecLock("SFQ",.T.)
                                        SFQ->FQ_FILIAL  := xFilial("SFQ")
                                        SFQ->FQ_ENTORI  := "SE2"
                                        SFQ->FQ_PREFORI := cPrefOri
                                        SFQ->FQ_NUMORI  := cNumOri
                                        SFQ->FQ_PARCORI := cParcOri
                                        SFQ->FQ_TIPOORI := cTipoOri
                                        SFQ->FQ_CFORI   := cCfOri
                                        SFQ->FQ_LOJAORI := cLojaOri

                                        SFQ->FQ_ENTDES  := "SE2"
                                        SFQ->FQ_PREFDES := SE2->E2_PREFIXO
                                        SFQ->FQ_NUMDES  := SE2->E2_NUM
                                        SFQ->FQ_PARCDES := SE2->E2_PARCELA
                                        SFQ->FQ_TIPODES := SE2->E2_TIPO
                                        SFQ->FQ_CFDES   := SE2->E2_FORNECE
                                        SFQ->FQ_LOJADES := SE2->E2_LOJA
                                        MsUnlock()
                                    EndIf
                                Next nLoop
                            EndIf
                            // Retorna do ponteiro do SE1 para a parcela
                            SE2->( MsGoto( nSavRec ) )
                            Reclock("SE2", .F. )

                            lRetParc := .T.
                        Case cModRetPIS == "3"
                            // Grava a Marca de "pendente recolhimento" dos demais registros
                            nRetOriPIS := nVlRetPis
                            nRetOriCOF := nVlRetCOF
                            nRetOriCSL := nVlRetCSL
                            If nRetOriPIS + nRetOriCof + nRetOriCsl > 0
                                Reclock("SE2", .F. )
                                SE2->E2_PRETPIS := "1"
                                SE2->E2_PRETCOF := "1"
                                SE2->E2_PRETCSL := "1"
                                SE2->( MsUnlock() )
                            EndIf
                            lRetParc := .F.
                            lRestValImp := .T.
                    EndCase
                ElseIf lPccBaixa .and. !lPccBxPa
                    Reclock("SE2", .F. )
                    SE2->E2_PRETPIS := "1"
                    SE2->E2_PRETCOF := "1"
                    SE2->E2_PRETCSL := "1"
                    SE2->( MsUnlock() )
                ElseIf lDigitado .and. !lPccBaixa
                    // Restauro o valor principal do titulo
                    //Controle de base de impostos
                    If lBaseImp
                        nVlPrinc := SE2->E2_BASEPIS
                    Else
                        nVlPrinc := M->E2_VALOR + nVlRetPis + nVlRetCOF + nVlRetCSL + M->E2_ISS + M->E2_IRRF + M->E2_INSS
                    EndIf

                    //PIS
                    If SED->ED_CALCPIS == "S" .and. SA2->A2_RECPIS == "2"
                        If ! GetNewPar("MV_RNDPIS",.F.)
                            nCalcPis := NoRound((nVlPrinc * (SED->ED_PERCPIS / 100)),2)
                        Else
                            nCalcPis := Round((nVlPrinc * (SED->ED_PERCPIS / 100)),2)
                        Endif
                    EndIf

                    // COFINS
                    If SED->ED_CALCCOF == "S" .and. SA2->A2_RECCOFI == "2"
                        If ! GetNewPar("MV_RNDCOF",.F.)
                            nCalcCof := NoRound((nVlPrinc * (SED->ED_PERCCOF / 100)),2)
                        Else
                            nCalcCof := Round((nVlPrinc * (SED->ED_PERCCOF / 100)),2)
                        EndIf
                    EndIf

                    // CSLL
                    If SED->ED_CALCCSL == "S" .and. SA2->A2_RECCSLL == "2"
                        If ! GetNewPar("MV_RNDCSL",.F.)
                            nCalcCsl := NoRound((nVlPrinc * (SED->ED_PERCCSL / 100)),2)
                        Else
                            nCalcCsl := Round((nVlPrinc * (SED->ED_PERCCSL / 100)),2)
                        EndIf
                    EndIf

                    // Guarda os valores originais
                    nRetOriPIS := nCalcPis
                    nRetOriCOF := nCalcCof
                    nRetOriCSL := nCalcCsl

                    // Exclui a Marca de "pendente recolhimento" dos demais registros
                    If aDadosRet[1] > 0
                        aRecnos := aClone( aDadosRet[ 5 ] )

                        cPrefOri  := SE2->E2_PREFIXO
                        cNumOri   := SE2->E2_NUM
                        cParcOri  := SE2->E2_PARCELA
                        cTipoOri  := SE2->E2_TIPO
                        cCfOri    := SE2->E2_FORNECE
                        cLojaOri  := SE2->E2_LOJA

                        For nLoop := 1 to Len( aRecnos )

                            SE2->( dbGoto( aRecnos[ nLoop ] ) )

                            RecLock("SE2",.F.)

                            SE2->E2_PRETPIS := "2"
                            SE2->E2_PRETCOF := "2"
                            SE2->E2_PRETCSL := "2"

                            SE2->( MsUnlock() )

                            If nSavRec <> aRecnos[ nLoop ]
                                dbSelectArea("SFQ")
                                RecLock("SFQ",.T.)
                                SFQ->FQ_FILIAL  := xFilial("SFQ")
                                SFQ->FQ_ENTORI  := "SE2"
                                SFQ->FQ_PREFORI := cPrefOri
                                SFQ->FQ_NUMORI  := cNumOri
                                SFQ->FQ_PARCORI := cParcOri
                                SFQ->FQ_TIPOORI := cTipoOri
                                SFQ->FQ_CFORI   := cCfOri
                                SFQ->FQ_LOJAORI := cLojaOri

                                SFQ->FQ_ENTDES  := "SE2"
                                SFQ->FQ_PREFDES := SE2->E2_PREFIXO
                                SFQ->FQ_NUMDES  := SE2->E2_NUM
                                SFQ->FQ_PARCDES := SE2->E2_PARCELA
                                SFQ->FQ_TIPODES := SE2->E2_TIPO
                                SFQ->FQ_CFDES   := SE2->E2_FORNECE
                                SFQ->FQ_LOJADES := SE2->E2_LOJA
                                MsUnlock()
                            EndIf
                        Next nLoop
                    EndIf
                    lRestValImp := .T.
                    lAltValor   := .F.
                    // Retorna do ponteiro do SE2 para a parcela
                    SE2->( MsGoto( nSavRec ) )
                    RecLock("SE2",.F.)
                Else
                    lRetParc := .T.
                EndIf
                If nRetOriPIS <> nVlRetPIS .and. !lPccBaixa  .And. !lEmpPub .And. !lF986Imp
                    lSisAltPIS := .T.
                EndIf

                If nRetOriCOF <> nVlRetCOF .and. !lPccBaixa .And. !lEmpPub .And. !lF986Imp
                    lSisAltCOF := .T.
                EndIf

                If nRetOriCSL <> nVlRetCSL .and. !lPccBaixa .And. !lEmpPub .And. !lF986Imp
                    lSisAltCSL := .T.
                EndIf
            EndIf

            SA2->(DbSeek(cFilial+SE2->E2_FORNECE+SE2->E2_LOJA))

            If !lEmprest
                //Gravar titulos de PCC
                a050DupPag(SE2->E2_ORIGEM,,,,lRetParc,,,,.T.,,,aMotRet,__lPccMR,__lIrfMR,__lInsMR,__lIssMR,__lCidMR,__lSestMR,Iif( __lRateioIR,__oRatIRF:aRatIRF, Nil))
            Else
                //realiza a gravacao do model do titulo emprestimo
                If __lLocBRA
                    Fa986grava("SE2","FINA050")
                EndIf
            EndIf
            If 	cPaisLoc $ "DOM|COS"  .And. !lF050Auto
                //Geração das Retenções de Impostos - Republica Dominicana
                //fa050CalcRet(cCarteira, cFatoGerador) //1-Contas a Pagar ou 3-Ambos e Fato Gerador 1-Emissao.
                fa050CalcRet("'1|3'", "2", SE2->E2_NATUREZ, SE2->E2_VALOR, SE2->E2_PREFIXO, SE2->E2_NUM, SE2->E2_FORNECE)
            EndIf
            If __lPccMR .Or. __lIrfMR
                Reclock("SE2", .F.)
                If __lPccMR
                    If __lPccBxMR
                        SE2->E2_PRETPIS := "1"
                        SE2->E2_PRETCOF := "1"
                        SE2->E2_PRETCSL := "1"
                    ElseIf Len(aMotRet) > 0
                        If nX := Ascan(aMotRet, {|x| AllTrim(x[8]) == "PIS"})
                            cPret := ""
                            If aMotRet[nX,5] == 0
                                cPret := "1"
                            EndIf
                            SE2->E2_PRETPIS := cPret
                        EndIf
                        If nX := Ascan(aMotRet, {|x| AllTrim(x[8]) == "COF"})
                            cPret := ""
                            If aMotRet[nX,5] == 0
                                cPret := "1"
                            EndIf
                            SE2->E2_PRETCOF := cPret
                        EndIf
                        If nX := Ascan(aMotRet, {|x| AllTrim(x[8]) == "CSL"})
                            cPret := ""
                            If aMotRet[nX,5] == 0
                                cPret := "1"
                            EndIf
                            SE2->E2_PRETCSL := cPret
                        EndIf
                    EndIf
                EndIf
                If __lIrfMR
                    If __lIrfBxMR
                        SE2->E2_PRETIRF := "1"
                    ElseIf Len(aMotRet) > 0
                        If nX := Ascan(aMotRet, {|x| AllTrim(x[8]) == "IRF"})
                            cPret := ""
                            If aMotRet[nX,5] == 0
                                cPret := "1"
                            EndIf
                            SE2->E2_PRETIRF := cPret
                        EndIf
                    EndIf
                EndIf
                SE2->( MsUnlock() )
            EndIf
            nValSaldo 	:= SE2->E2_VALOR
            nMoedSE2 	:= SE2->E2_MOEDA
        EndIf

        // Atualiza Saldos do Fornecedor
        If !lDesdobr
            dbSelectArea("SA2")
            dbGoto(nSavRecA2)
            SE2->(dbGoTo(nSavRec))
            If SE2->E2_TIPO $ MVABATIM
                Reclock("SA2" )
                SA2->A2_SALDUP -= SE2->E2_VLCRUZ
                SA2->A2_SALDUPM-= Round(NoRound(xMoeda(SE2->E2_VALOR,SE2->E2_MOEDA,nMoeda,SE2->E2_EMISSAO,3),3),2)
            EndIf
        Else
            // Atualiza datas de primeira e ultima compra
            SA2->(dbGoto(nSavRecA2))
            SE2->(dbGoTo(nSavRec))
            RecLock("SA2", .F. )
            SA2->A2_PRICOM  := Iif( SE2->E2_EMISSAO < SA2->A2_PRICOM .Or. Empty(SA2->A2_PRICOM), SE2->E2_EMISSAO, SA2->A2_PRICOM )
            SA2->A2_ULTCOM  := Iif( SA2->A2_ULTCOM  < SE2->E2_EMISSAO, SE2->E2_EMISSAO, SA2->A2_ULTCOM )
            SA2->( MsUnlock() )
        Endif
        // Ponto de entrada do FA050GRV, serve p/ tratar dados
        // ap¢s estarem gravados.
        IF lFA050GRV
            ExecBlock("FA050GRV",.f.,.f.)
        EndIf

        // Valores acessórios
        If __lLocBRA
            lVincVA := (Valtype(mv_par11) == "N" .AND. !Empty(mv_par11) .And. mv_par11 == 1)
            If !lDesdobr .And. ( lVincVA .Or. __aVAAuto != NIL ) .And. lFAPodeTVA .And. FAPodeTVA(SE2->E2_TIPO, /*cNatureza*/,.F.,"P")
                If lF050Auto
                    If (__aVAAuto != NIL)
                        If !Fa050VA(.T.)
                            lRet := .F.
                        Endif
                    Endif
                Else
                    Fa050VA(.F.)
                Endif
            Endif
        Endif
    Endif

    If lRet
        // Rotina de contabiliza‡„o do titulo
        dbSelectArea("SE2")
        dbGoto( nSavRec )

        // Verifica se esta utilizando multiplas naturezas
        If MV_MULNATP .And. SE2->E2_MULTNAT == "1"   .And. !SE2->E2_DESDOBR $ "1S"
            // Se o parametro que permite a exibicao da tela para digitacao
            // do rateio estiver ativo, concatena a rotina de digitacao de multiplas naturezas (MultNat2)
            If Type("lF050Auto") == "L" .And. lF050Auto .AND. SuperGetMv("MV_RATAUTO",,.F.)
                // Grava as multiplas naturezas (SEV e SEZ)
                MultNat(	"SE2" /*cAlias*/,;
                @nHdlPrv /*@nHdlPrv*/,;
                @nTotal /*@nTotal*/,;
                @cArquivo /*@cArquivo*/,;
                ( mv_par04 == 1 ) /*lContabiliza*/,;
                /*nOpc*/,;
                If(	/*lExpr*/	mv_par06 == 1,;
                /*T*/	SE2->(	If( lIRPFBaixa, 0, E2_IRRF ) + If( !lCalcIssBx, E2_ISS, 0 ) +;
                E2_RETENC + E2_SEST +;
                If( lPccBaixa, 0, E2_PIS + E2_COFINS + E2_CSLL ) ) + nInss,;
                /*F*/	0 ) /*nImpostos*/,;
                mv_par10 = 2 .And. mv_par06 = 2 /*lRatImpostos*/,;
                /*aColsM*/,;
                /*aHeaderM*/,;
                /*aRegs*/,;
                /*lGrava*/,;
                /*lMostraTela*/,;
                /*lRotAuto*/,;
                lUsaFlag /*lUsaFlag*/,;
                @aFlagCTB /*@aFlagCTB*/ ) // Chama a rotina para distribuir o valor entre as naturezas
            Else
                If lf050auto .and. aRatEvEz <> Nil
                    MultiAuto(@aColsSev,@aHeaderSev,"SE2","SEV")
                Endif
                If !GrvSevSez(cAlias,aColsSev,aHeaderSev,,;
                    If(mv_par06 == 1,If(lIRPFBaixa,0,M->E2_IRRF)+If(!lCalcIssBx,M->E2_ISS,0)+nInss+IIF(lPccBaixa,0,M->E2_PIS+M->E2_COFINS+M->E2_CSLL)+;
                    M->E2_RETENC+M->E2_SEST,0),(mv_par10 == 2 .And. mv_par06 = 2),"FINA050",mv_par04==1,@nHdlPrv,@nTotal,@cArquivo)

                    lRet := .F.
                Endif
            Endif
            lHeader := nHdlPrv > 0
        Else
            If lAtuSldNat .And. !lDesdobr  .And. SE2->E2_FLUXO == 'S'
                AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, If(SE2->E2_TIPO $ MVPAGANT+"/"+MV_CPNEG,"3","2"), "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, If(SE2->E2_TIPO $ MVABATIM,"-","+"),,FunName(),"SE2",SE2->(Recno()),3)
            Endif
        Endif
    Endif

    If lRet
        //PONTO DE ENTRADA - Apos gravar SEV e SEZ e antes de gravar lcto contabil
        If lFA050CT
            ExecBlock("FA050CT",.F.,.F.)
        EndIf

        // Atualizacao dos dados do Modulo SIGAPMS
        lPrimeiro := .F. //Wilson em 06/06/2011
        If IntePMS()
            PmsWriteFI(1,"SE2")
        Endif
        STRLCTPAD := " "
        cPadrao:=IIF(SE2->E2_RATEIO=="N","510","511")
        IF !E2_TIPO $ MVPROVIS .or. mv_par02 == 1
            If !lDesdobr   // Caso n„o seja titulo gerado por desdobramento
                IF E2_TIPO $ MVPAGANT .and. cPadrao <> "511"
                    // O PA sera contabilizado com o Lanc Padrao da orden de pago.|
                    //| (Localizacoes Argentina).                                  |
                    cPadrao:=IIf(cPaisLoc $ "ARG|ANG|MEX|COL","ZZZZZ","513") //que nao faza o lancamento se e Um PA em argentina
                    STRLCTPAD := cBancoAdt+"/"+cAgenciaAdt+"/"+cNumCon+"/"+cChequeAdt
                Endif
                lPadrao:=VerPadrao(cPadrao)
                // VALIDA CONTABILIZAÇÃO DE PAGAMENTO ANTECIPADO
                // Nao contabilizar inclusao de PA sem Cheque e Movimento bancario, exceto quando o banco pertence ao MV_CARTEIR
                If lCtMovPa .And. cPadrao == "513" .And. cBancoAdt <> cBancoCx
                    If MV_PAR05 == 2 .And. MV_PAR09 == 2
                        lPadrao := .F.
                    EndIf
                EndIf
            EndIf

            // Adiciona o recno no array para a contabilização
            If  UsaSeqCor()
                aadd(aDiario, {"SE2", SE2->(recno()) , SE2->E2_DIACTB , "E2_NODIA","E2_DIACTB"} )
            Else
                aDiario := {}
            Endif

            IF ((lPadrao .Or. lDesdobr) .and. SE2->E2_LA != "S") .or. (SE2->E2_LA == "S" .and. lDesdobr)
                // Deve sempre mostrar a tela de rateio
                // Caso o titulo tenha sua origem num desdobramento
                // n„o haver  possibilidade de Rateio.
                If cPadrao == "511" .And. !lDesdobr .And. !__lRatDes
                    // Contabiliza o rateio
                    cSeq := Fa050GerLc( cPadrao,cLote, "FINA050", 3, @nHdlPrv, @nTotal, NIL, cProcPCO, cItemPCO, cRecPag )
                    If !Empty(cSeq)
                        RecLock("SE2")
                        Replace E2_ARQRAT		With cSeq
                    EndIf
                ElseIf mv_par04 == 1  // Contabiliza On-Line
                    // Se houve desdobramento, n„o rodo o HeadProva nem o
                    // DetProva, pois j  foram feitos na gravacao dos titu-
                    // los gerados pelo desdobramento.
                    If E2_TIPO $ MVPAGANT
                        SA6->( dbSeek(xFilial("SA6")+cBancoAdt+cAgenciaAdt+cNumCon) )
                    Endif
                    If !lDesdobr

                        If SE2->E2_TIPO $ MVPAGANT+"/"+MV_CPNEG
                            If FwIsInCallStack("Fa050Subst") //Geração via substituição atribui valores diretamente na memória, logo não executa o X3_VALID dos campos
                                FA050Nat2()
                            Endif
                            Fa050GerPa(cBancoCx,lPadrao,__lPccMR,__lIrfMR,__lInsMR,__lIssMR,__lCidMR,__lSestMR,Iif( __lRateioIR,__oRatIRF:aRatIRF, Nil),cItnUuidBs,__aTitCalc)
                        Endif

                        If SE2->E2_MULTNAT == "1"
                            SEV->(DbGoto(0)) // Desposiciona SEV para contabilizar as demais sequencias do LP 510
                            SEZ->(DbGoto(0)) // Desposiciona SEZ para contabilizar as demais sequencias do LP 510
                        Endif

                        If nHdlPrv <= 0
                            // Inicializa Lancamento Contabil
                            nHdlPrv := HeadProva( cLote,;
                            "FINA050" /*cPrograma*/,;
                            Substr(cUsuario,7,6),;
                            @cArquivo )
                        Endif

                        //Atribui valor as variáveis de contab. motor de retenção
                        If nHdlPrv != 0 .And. Len(aMotRet) > 0
                            FinCarVarE(aMotRet)
                        EndIf

                        // Prepara Lancamento Contabil
                        If lUsaFlag  // Armazena em aFlagCTB para atualizar no modulo Contabil
                            aAdd( aFlagCTB, {"E2_LA", "S", "SE2", SE2->( Recno() ), 0, 0, 0} )
                        Endif
                        nTotal += DetProva( nHdlPrv,;
                        cPadrao,;
                        "FINA050" /*cPrograma*/,;
                        cLote,;
                        /*nLinha*/,;
                        /*lExecuta*/,;
                        /*cCriterio*/,;
                        /*lRateio*/,;
                        /*cChaveBusca*/,;
                        /*aCT5*/,;
                        /*lPosiciona*/,;
                        @aFlagCTB,;
                        /*aTabRecOri*/,;
                        /*aDadosProva*/ )
                    Endif
                    If nTotal > 0
                        //-- Se for rotina automatica força exibir mensagens na tela, pois mesmo quando não exibe os lançamentos, a tela
                        //-- sera exibida caso ocorram erros nos lançamentos padronizados
                        If lF050Auto
                            lSetAuto := _SetAutoMode(.F.)
                            lSetHelp := HelpInDark(.F.)
                            If Type('lMSHelpAuto') == 'L'
                                lMSHelpAuto := !lMSHelpAuto
                            EndIf
                        EndIf

                        // Envia para Lan‡amento Contábil
                        cA100Incl( cArquivo,;
                        nHdlPrv,;
                        3 /*nOpcx*/,;
                        cLote,;
                        ( mv_par01 == 1 ) /*lDigita*/,;
                        ( mv_par07 == 1 ) /*lAglut*/,;
                        /*cOnLine*/,;
                        /*dData*/,;
                        /*dReproc*/,;
                        @aFlagCTB,;
                        /*aDadosProva*/,;
                        aDiario )
                        aFlagCTB := {}  // Limpa o coteudo apos a efetivacao do lancamento

                        If lF050Auto
                            HelpInDark(lSetHelp)
                            _SetAutoMode(lSetAuto)
                            If Type('lMSHelpAuto') == 'L'
                                lMSHelpAuto := !lMSHelpAuto
                            Endif
                        EndIf

                        If !lUsaFlag .Or. lDesdobr
                            dbSelectArea("SE2")
                            // Atualiza flag de Lan‡amento Contábil
                            Reclock("SE2")
                            Replace E2_LA With "S"
                            SE2->( MsUnlock() )
                        EndIf
                    Endif
                EndIf
                If !lDesdobr .and. !SE2->E2_TIPO $ MVTAXA+"/"+MVINSS+"/"+MVISS+"/"+"SES"+"CID"
                    // Atualiza flag de Lan‡amento Contábil dos titulos de impostos, para nao
                    // duplicar o lancamento na contabilizacao off-line, pois os valores
                    // destes impostos estao disponiveis no mesmo registro do titulo principal
                    dbSelectArea("SE2")
                    nRecCtb := Recno()
                    aTps := {"TX ","INS","ISS","SES"}
                    aParc := {SE2->E2_PARCIR,SE2->E2_PARCINS,SE2->E2_PARCISS,SE2->E2_PARCSES}
                     if __lLocBRA
					    Aadd(aParc, SE2->E2_PARCCID)
                        Aadd(aTps , "CID")
                    Endif                    
                    Aadd(aParc, SE2->E2_PARCPIS)
                    Aadd(aParc, SE2->E2_PARCCOF)
					Aadd(aParc, SE2->E2_PARCSLL)
                    Aadd(aTps , "TX ")
                    Aadd(aTps , "TX ")
                    Aadd(aTps , "TX ") // aTps deve ter o mesmo tamanho de aParc
                    For nX := 1 to Len(aTps)
                        If Dbseek(xFilial("SE2")+SE2->E2_PREFIXO+SE2->E2_NUM+aParc[nX]+aTps[nX])
                            Reclock("SE2")
                            Replace E2_LA With "S"
                        Endif
                        dbGoto(nRecCtb)
                    Next
                EndIf
            EndIf
        Else
            // Gera LP 511 somente quando NAO FOI EFETUADO DESDOBRAMENTO, caso contrario o LP ja foi gerado no desdobramento
            If cPadrao == "511" .And. SE2->E2_DESDOBR != "S"
                // Contabiliza o rateio
                cSeq := Fa050GerLc( cPadrao,cLote, "FINA050", 3, @nHdlPrv, @nTotal, NIL, cProcPCO, cItemPCO, cRecPag )
                If !Empty(cSeq)
                    RecLock("SE2")
                    Replace E2_ARQRAT		With cSeq
                EndIf
            Endif
        Endif

        If !__lPccMR .And. lRestValImp
            // Restaura os valores originais de PIS / COFINS / CSLL
            RecLock( "SE2", .F. )
            If M->E2_PIS == 0 .or. lSisAltPIS
                SE2->E2_PIS    := Iif((!Empty(nRetOriPIS) .And. !lDigitado) ,nRetOriPIS,SE2->E2_PIS)
            ElseIf lDigitado .And. aDadosRet[2] > 0
                SE2->E2_PIS    -= If(nRetOriPIS < SE2->E2_PIS   ,nRetOriPIS,0)
            EndIf

            If M->E2_COFINS == 0 .or. lSisAltCOF
                SE2->E2_COFINS := Iif((!Empty(nRetOriCOF) .And. !lDigitado),nRetOriCOF,SE2->E2_COFINS)
            ElseIf lDigitado .And. aDadosRet[3] > 0
                SE2->E2_COFINS -= If(nRetOriCOF < SE2->E2_COFINS,nRetOriCOF,0)
            EndIf
            
            If M->E2_CSLL == 0 .or. lSisAltCSL
                SE2->E2_CSLL   := Iif((!Empty(nRetOriCSL) .And. !lDigitado),nRetOriCSL,SE2->E2_CSLL)
            ElseIf lDigitado .And. aDadosRet[4] > 0
                SE2->E2_CSLL   -= If(nRetOriCSL < SE2->E2_CSLL  ,nRetOriCSL,0)
            EndIf

            SE2->E2_VLCRUZ := Round(NoRound(xMoeda(SE2->E2_VALOR,SE2->E2_MOEDA,1,SE2->E2_EMISSAO,MsDecimais(1)+1,SE2->E2_TXMOEDA),MsDecimais(1)+1),MsDecimais(1))
        EndIf
        If SE2->E2_TIPO $ MVPAGANT+"/"+MV_CPNEG .and. ((!lPadrao .and. mv_par04==1) .or.  mv_par04==2 .or. (lPadrao .and. cPadrao == "511" .And. !lDesdobr) )
            If FwIsInCallStack("Fa050Subst") //Geração via substituição atribui valores diretamente na memória, logo não executa o X3_VALID dos campos
                FA050Nat2()
            Endif
            Fa050GerPa(cBancoCx,,__lPccMR,__lIrfMR,__lInsMR,__lIssMR,__lCidMR,__lSestMR,Iif( __lRateioIR,__oRatIRF:aRatIRF, Nil),cItnUuidBs,__aTitCalc)
        Endif

        If cPaisLoc $ "ARG|ANG|MEX|COL" .and. SE2->E2_TIPO $ MVPAGANT
            // Se for PA, gera Ordem de pagamento.
            DbSelectArea("SX5")
            DbSeek(xFilial("SX5")+"99"+"ORDPAG")
            If Found()
                cOrdPago :=	StrZero(Val(AllTrim(X5Descri())) + 1, TamSX3("EK_ORDPAGO")[1])
                FWPutSX5("", "99", "ORDPAG", cOrdPago, cOrdPago, cOrdPago, cOrdPago)
            Else
            	If !IsBlind()
            		MsgInfo(STR0365) //"No existe la clave ORDPAG en la Tabla Genérica 99 - Numeración de Documentos Válidos (SX5). Regístrela e intente nuevamente."
            	Else
            		ConOut(STR0365) //"No existe la clave ORDPAG en la Tabla Genérica 99 - Numeración de Documentos Válidos (SX5). Regístrela e intente nuevamente."
                EndIf
                lRet := .F.
            EndIf

            If GetMv("MV_LIBCHEQ") == "S".And. ;
                (Subs(cBancoAdt,1,2)=="CX" .or. cBancoAdt $ cBancoCx )
                    aAreaAnt := GetArea()
                    oModel := FWLoadModel('FINM030')//Movimento Bancario
                    oModel:SetOperation( 4 ) //Alteração
                    oModel:Activate()
                    oModel:SetValue( "MASTER", "E5_GRV", .T. ) //habilita gravação de SE5

                    oSubFKA := oModel:GetModel( "FKADETAIL" )
                    oSubFKA:SeekLine( 	{ {"FKA_IDORIG", SE5->E5_IDORIG } } )

                    //Dados do movimento
                    oSubFK5 := oModel:GetModel( "FK5DETAIL" )
                    oSubFK5:SetValue( "FK5_ORDREC", cOrdPago )

                    If oModel:VldData()
                        oModel:CommitData()
                        oModel:DeActivate()
                    Else
                        cLog := cValToChar(oModel:GetErrorMessage()[4]) + ' - '
                        cLog += cValToChar(oModel:GetErrorMessage()[5]) + ' - '
                        cLog += cValToChar(oModel:GetErrorMessage()[6])

                        If (Type("lF050Auto") == "L" .and. lF050Auto)
                            Help( ,,"M050VALID",,cLog, 1, 0 )
                        Endif
                        lRet := .F.
                    Endif
                Restarea(aAreaAnt)
            Endif

            If lRet
                RecLock("SEK",.T.)
                SEK->EK_FILIAL	:= xFilial("SEK")
                SEK->EK_TIPODOC := "PA" //CHEQUE PROPRIO
                SEK->EK_NUM     := SE2->E2_NUM
                SEK->EK_TIPO    := "PA"
                SEK->EK_FORNECE := SE2->E2_FORNECE
                SEK->EK_LOJA	:= SE2->E2_LOJA
                SEK->EK_EMISSAO := dDataBase
                SEK->EK_VENCTO  := dDatabase
                SEK->EK_VALOR   := SE2->E2_VALOR
                SEK->EK_SALDO   := SE2->E2_VALOR
                SEK->EK_VLMOED1 := SE2->E2_VLCRUZ
                SEK->EK_MOEDA	:= STRZERO(SE2->E2_MOEDA,2)
                SEK->EK_ORDPAGO := cOrdpago
                SEK->EK_DTDIGIT := dDataBase
                MSUNLOCK()

                RecLock("SE2",.T.)
                SE2->E2_FILIAL 	:= cFilial
                SE2->E2_BCOCHQ	:= cBancoAdt
                SE2->E2_AGECHQ	:= cAgenciaAdt
                SE2->E2_CTACHQ	:= cNumCon
                SE2->E2_NUM		:= cChequeAdt
                SE2->E2_EMISSAO	:=	If(SubStr(cBancoAdt,1,2) != "CX" .and. !(cBancoAdt$cBancoCx),SEF->EF_DATA,SE5->E5_DATA)
                SE2->E2_VENCTO 	:=	If(SubStr(cBancoAdt,1,2) != "CX" .and. !(cBancoAdt$cBancoCx),SEF->EF_VENCTO,SE5->E5_DTDISPO)
                SE2->E2_VENCREA	:=	DataValida(If(SubStr(cBancoAdt,1,2) != "CX" .and. !(cBancoAdt$cBancoCx),SEF->EF_VENCTO,SE5->E5_VENCTO))
                SE2->E2_VALOR	:= If(SubStr(cBancoAdt,1,2) != "CX" .and. !(cBancoAdt$cBancoCx),SEF->EF_VALOR,SE5->E5_VLMOED2)
                SE2->E2_SALDO	:= If(SubStr(cBancoAdt,1,2) != "CX" .and. !(cBancoAdt$cBancoCx),SEF->EF_VALOR,SE5->E5_VLMOED2)
                SE2->E2_NATUREZ	:= SA2->A2_NATUREZ
                SE2->E2_TIPO	:= "CH"
                SE2->E2_LA		:= "S"
                SE2->E2_NOMFOR  := iif(Empty(cBenef),SEF->EF_BENEF,SA2->A2_NOME)
                SE2->E2_PREFIXO	:= If(SubStr(cBancoAdt,1,2) != "CX" .and. !(cBancoAdt$cBancoCx),SEF->EF_PREFIXO,SE5->E5_PREFIXO)
                SE2->E2_FORNECE := If(SubStr(cBancoAdt,1,2) != "CX" .and. !(cBancoAdt$cBancoCx),SEF->EF_FORNECE,SE5->E5_CLIFOR)
                SE2->E2_LOJA	:= If(SubStr(cBancoAdt,1,2) != "CX" .and. !(cBancoAdt$cBancoCx),SEF->EF_LOJA,SE5->E5_LOJA)
                SE2->E2_EMIS1	:= IIf(Type("dDataEmis1") # "U", IIf(!Empty(dDataEmis1),dDataEmis1,dDataBase),dDataBase)
                SE2->E2_VLCRUZ 	:= Round(NoRound(xMoeda(If(SubStr(cBancoAdt,1,2) != "CX" .and. !(cBancoAdt$cBancoCx),SEF->EF_VALOR,SE5->E5_VLMOED2),nMoedSE2,1,DdATABASE,MsDecimais(1)+1),MsDecimais(1)+1),MsDecimais(1))
                SE2->E2_MOEDA	:=	nMoedSE2
                MsUnLock()

                RecLock("SEK",.T.)
                SEK->EK_FILIAL	:= cFilial
                SEK->EK_TIPODOC := "CP" //CHEQUE PROPRIO
                SEK->EK_PREFIXO := SE2->E2_PREFIXO
                SEK->EK_NUM     := cChequeAdt
                SEK->EK_TIPO    := "CH"
                SEK->EK_FORNECE := SE2->E2_FORNECE
                SEK->EK_LOJA	:= SE2->E2_LOJA
                SEK->EK_EMISSAO := dDataBase
                SEK->EK_VENCTO  := dDatabase
                SEK->EK_VALOR   := SE2->E2_VALOR
                SEK->EK_SALDO   := SE2->E2_VALOR
                SEK->EK_VLMOED1 := SE2->E2_VLCRUZ
                SEK->EK_MOEDA	:= STRZERO(SE2->E2_MOEDA,2)
                SEK->EK_BANCO   := cBancoAdt
                SEK->EK_AGENCIA := cAgenciaAdt
                SEK->EK_CONTA   := cNumCon
                SEK->EK_ORDPAGO := cOrdpago
                SEK->EK_DTDIGIT := dDataBase
                MsUnlock()
                cPadrao:="570"
                lPadrao:=VerPadrao(cPadrao)
                If lPadrao .and. mv_par04 == 1 // Contabiliza On-Line
                    IF !lHeader
                        // Inicializa Lancamento Contabil
                        nHdlPrv := HeadProva( cLote,;
                        "FINA050" /*cPrograma*/,;
                        Substr(cUsuario,7,6),;
                        @cArquivo )
                        lHeader := .T.
                    Endif
                    SEK->(DbSetOrder(1))
                    SEK->(DbSeek(xFilial("SEK")+cOrdPago,.T.))
                    While !SEK->(EOF()).And.SEK->EK_ORDPAGO==cOrdPago
                        If ( SEK->EK_TIPODOC=="CP" )
                            SA6->(DbsetOrder(1))
                            SA6->(DbSeek(xFilial("SA6")+SEK->EK_BANCO+SEK->EK_AGENCIA+SEK->EK_CONTA,.F.))
                        Endif

                        // Prepara Lancamento Contabil
                        If lUsaFlag  // Armazena em aFlagCTB para atualizar no modulo Contabil
                            aAdd( aFlagCTB, {"EK_LA", "S", "SEK", SEK->( Recno() ), 0, 0, 0} )
                        EndIf
                        nTotal += DetProva( nHdlPrv,;
                        "570" /*cPadrao*/,;
                        "FINA050" /*cPrograma*/,;
                        cLote,;
                        /*nLinha*/,;
                        /*lExecuta*/,;
                        /*cCriterio*/,;
                        /*lRateio*/,;
                        /*cChaveBusca*/,;
                        /*aCT5*/,;
                        /*lPosiciona*/,;
                        @aFlagCTB,;
                        /*aTabRecOri*/,;
                        /*aDadosProva*/ )

                        If !lUsaFlag
                            RecLock("SEK",.F.)
                            Replace EK_LA With "S"
                            MsUnLock()
                        EndIf

                        SEK->(DbSkip())
                    Enddo
                    // Adiciona o recno no array para a contabilização
                    If  UsaSeqCor()
                        aadd(aDiario, {"SE2", SE2->(recno()) , SE2->E2_DIACTB , "E2_NODIA","E2_DIACTB"} )
                    Else
                        aDiario := {}
                    EndIf
                    // Envia para Lan‡amento Contábil
                    cA100Incl( cArquivo,;
                    nHdlPrv,;
                    3 /*nOpcx*/,;
                    cLote,;
                    ( mv_par01 == 1 ) /*lDigita*/,;
                    ( mv_par07 == 1 ) /*lAglut*/,;
                    /*cOnLine*/,;
                    /*dData*/,;
                    /*dReproc*/,;
                    @aFlagCTB,;
                    /*aDadosProva*/,;
                    aDiario )
                    aFlagCTB := {}  // Limpa o coteudo apos a efetivacao do lancamento
                EndIf
            Endif
        EndIf

        If lRet
            // Ponto de entrada do FA050FIN, serve p/ tratar dados
            // antes de sair da rotina.
            IF lFA050FIN
                ExecBlock("FA050FIN",.f.,.f.)
            Endif

            // Grava os lancamentos nas contas orcamentarias SIGAPCO
            // Grava os lancamentos nas contas orcamentarias quando nao eh desdobramento - SIGAPCO
            If !lDesdobr .And. SE2->E2_MULTNAT # "1"
                If SE2->E2_TIPO $ MVPAGANT
                    PcoDetLan("000002","02","FINA050")	// Tipo PA
                Else
                    PcoDetLan("000002","01","FINA050")
                EndIf
            EndIf

            // Verifica o arquivo de rateio, e deleta o conteúdo do arquivo temporario
            // para que no proximo rateio seja reutilizado a mesma tabela no banco
            If __lLocBRA .And. !__lRatDes
                If SELECT("TMP") > 0 .And. !Empty(__cFIN1Name)
                    nTcSql := TcSQLExec("DELETE FROM "+__cFIN1Name)
                    FChkTCExec(nTcSql, 1)
                EndIf
            EndIf

            cBancoAdt	:= CriaVar("A6_COD")
            cAgenciaAdt := CriaVar("A6_AGENCIA")
            cNumCon		:= CriaVar("A6_NUMCON")
            cChequeAdt	:= CriaVar("EF_NUM")
            cHistor		:= CriaVar("EF_HIST")
            cBenef		:= CriaVar("EF_BENEF")

            // Adiciona botao para envio de instrucoes de cobranca
            F050GrvFI2()
  
            //-------------------------------------------------------------------------------------
            // Integração Gesplan - Update somente para atualização do timestamp do registro pai
            If __lGesplan .And. ( SE2->E2_TIPO $ MVABATIM )		
                SE2->(dbSetOrder(1))
                If !Empty(SE2->E2_TITPAI) .And. SE2->(MsSeek(xFilial("SE2") + SE2->E2_TITPAI))
                    FUpdStamp('SE2',SE2->(Recno()))
                EndIF	
                SE2->(MsGoto(nSavRec)) //reposiciono no AB-
            EndIF            

            // Restaura filial caso a importacao MILE tenha gestao corporativa
            If lMile .And. !Empty(cFilAux) .And. cFilAnt <> cFilAux
                cFilAnt := cFilAux
            EndIf
            
            dbSelectArea(cAlias)
            dbSetOrder(nIndex)

            If nSavRec >0
                dbGoto(nSavRec)
            Endif

            If __lNRasDSD .and. SE2->E2_DESDOBR == 'S'
                If !IsBlind()
                    oMBrowse:Refresh()
                EndIf
                SE2->(DbGoto(nSavRec + 1))
            EndIf

            // Integração com o SigaPfs
            If __lIntPFS
                If !F050AtuPFS(3, nSavRec, SE5->(Recno()))
                    lRet := .F.
                EndIf
            Endif
        Endif
    Endif

    If !lRet
        DisarmTransaction()
        Break
    Endif

Return  /*FA050AxInc*/

//-------------------------------------------------------
/*/{Protheus.doc} FA050AxAlt

Função para complementacao da Alteracao de C.Pagar

@author Mauricio Pequim Jr.
@since 04/08/99
@version P12
/*/
//-------------------------------------------------------
Function FA050AxAlt(cAlias As Character) As Logical

    Local lResult		As Logical
    LOCAL dVencRea      As Date 
    LOCAL dVenIss       As Date 
    LOCAL nValorIss		As Numeric
    LOCAL cNum 			As Character
    LOCAL cPrefixo 		As Character
    LOCAL nValorIr  	As Numeric
    LOCAL nValInss  	As Numeric
    LOCAL nValSEST    	As Numeric
    Local cTipoSE2		As Character
    Local cModSpb		As Character
    Local lSpbInUse		As Logical
    LOCAL nTotal		As Numeric
    LOCAL nHdlPrv		As Numeric
    Local cArquivo		As Character
    Local nTamParc  	As Numeric
    Local lAltLib		As Logical
    Local dVctoReal		As Date
    Local dEmissao		As Date
    Local dEmis1		As Date
    Local nValPis       As Numeric
    Local nValCofins    As Numeric
    Local nValCsll      As Numeric
    Local cParcPis      As Character
    Local cParcCof      As Character
    Local cParcCsll     As Character
    Local nX            As Numeric
    Local cGeraDirf     As Character
    Local cCodRetIr     As Character
    Local cCodRetPis    As Character
    Local cCodRetCof    As Character
    Local cCodRetCsl    As Character
    Local cChavePIS     As Character
    Local cChaveCOF     As Character
    Local cChaveCSL     As Character
    Local cChaveInss    As Character
    Local cChaveIss     As Character
    Local cCIDE         As Character
    Local nRegSe2	    As Numeric
    Local cUniao	    As Character
    Local cForInss	    As Character
    Local aRecnos       As Array
    Local nLoop         As Numeric
    Local nSobra        As Numeric
    Local nValorTit     As Numeric
    Local nRetOriPIS    As Numeric 
    Local nRetOriCOF    As Numeric
    Local nRetOriCSL    As Numeric 
    Local nVlMinImp     As Numeric
    Local nVlMinPcc     As Numeric 
    Local lRestValImp   As Logical 
    Local lRetParc      As Logical 
    Local nInss         As Numeric
    Local cPrefOri      As Character
    Local cNumOri       As Character
    Local cParcOri      As Character
    Local cTipoOri      As Character
    Local cCfOri        As Character
    Local cLojaOri      As Character
    Local lZerouImp     As Logical 

    //Controla o Pis Cofins e Csll na baixa
    Local lPCCBaixa     As Logical 

    //1-Cria NCC/NDF referente a diferenca de impostos entre emitidos (SE2) e retidos (SE5)
    //2-Nao Cria NCC/NDF, ou seja, controla a diferenca num proximo titulo
    //3-Nao Controla
    Local cNccRet       As Character
    Local lIRPFBaixa    As Logical
	Local lCalcIssBx    As Logical
	Local lCIDE  	    As Logical
    Local cTipoFor 		As Character
    //Como a variavel cTipoFor eh destinada mais especificamente ao calculo de IRRF (pelo campo A2_IRPROG) utilizar outra para outros impostos como o INSS
    Local cTipoFor02	As Character
    Local nRegSeD       As Numeric
    Local aTpImp        As Array
    Local aFlagCTB      As Array
    Local lUsaFlag      As Logical
    Local dDataIni      As Date
    Local dDataFim      As Date 
    Local cTitPai       As Character
    Local cCodAprov     As Character
    Local cLojaImp      As Character
    Local aAreaSed      As Array
    Local aAreaSa2      As Array
    Local dVencto       As Date
    Local cKeySe2       As Character
    Local cFornLoja     As Character
    Local lVerMinIss	As Logical
    Local cForMinISS	As Character
    Local lGravRegIss	As Logical
    Local lVcAntIss 	As Logical 
    Local lEmpPub		As Logical 
    Local lF050ISS 		As Logical 
    Local lF050SES		As Logical
    Local lAtuSldNat    As Logical
    Local lTitReteu     As Logical 
    Local lRatAutPrj	As Logical
    Local cForSEST      As Character
    Local lRetOutMod	As Logical 
    Local cRetIns   	As Character
    Local cTpTaxa       As Character
    Local nTotImp       As Numeric
    Local lRndSest      As Logical
    Local aForISSCPM    As Array
    Local aImpos        As Array
    Local aTitImp       As Array
    Local nY            As Numeric
    Local aRecImpos     As Array
    Local aImps         As Array
    Local cNatIrf       As Character
    Local aDeleta       As Array
    Local nBaseIrrf     As Numeric
    LOCAL cSEST         As Numeric 
    Local lDiaUIss      As Numeric 

    //Contabilização do Rateio
    Local cProcPCO		As Character
    Local cItemPCO		As Character
    Local cRecPag 		As Character
    Local cPadrao 		As Character
    Local lPadrao 		As Logical
    Local cSeq			As Character

    PRIVATE bPMSDlgFI As Block 

    Default __lIntPFS  := SuperGetMv("MV_JURXFIN",.T.,.F.) //Integração do Financeiro com o Juridico(Habilitado = .T.)
    Default __lFnBtr   := FindFunction("ISSCPOM") .And. FindFunction("BtrISSMun")
    Default __lBtrISS  := SE2->(ColumnPos("E2_BTRISS")) > 0 .And. SE2->(ColumnPos("E2_VRETBIS")) > 0 .And. SE2->(ColumnPos("E2_CODSERV")) > 0 .And. __lFnBtr
    Default __lFNCDRET := ExistBlock("FINCDRET")
    Default __lTemMR   := (FindFunction("FTemMotor") .and. FTemMotor())

    lIntegracao := IF(GetMV("MV_EASYFIN")=="S",.T.,.F.)
    
    If __lGesplan == Nil
        __lGesplan  := SuperGetMv("MV_FINTGES",.F.,.F.) .And. FindFunction("FUpdStamp")
    EndIF

    lResult	   := .T.
    dVencRea   := CTOD("//")
    dVenIss    := CTOD("//")
    nValorIss  := 0
    cNum 	   := E2_NUM
    cPrefixo   := E2_PREFIXO
    nValorIr   := 0
    nValInss   := 0
    nValSEST   := 0
    cTipoSE2   := SE2->E2_TIPO
    cModSpb    := "1"
    lSpbInUse  := SpbInUse()
    nTotal	   := 0
    nHdlPrv	   := 0
    cArquivo   := ""
    nTamParc   := TamSx3("E2_PARCELA")[1]
    lAltLib	   := .T.
    dVctoReal  := SE2->E2_VENCREA
    dEmissao   := SE2->E2_EMISSAO
    dEmis1	   := SE2->E2_EMIS1
    nValPis    := 0
    nValCofins := 0
    nValCsll   := 0
    cParcPis   := ""
    cParcCof   := ""
    cParcCsll  := ""
    nX         := 0
    cGeraDirf  := Iif(__lLocBRA,SE2->E2_DIRF," ")
    cCodRetIr  := Iif(cPaisLoc $ "ANG|ARG|AUS|BOL|BRA|CHI|COL|COS|DOM|EQU|EUA|HAI|MEX|PAD|PAN|PAR|PER|POR|PTG|SAL|TRI|URU|VEN",SE2->E2_CODRET," ")
    cCodRetPis := ""
    cCodRetCof := ""
    cCodRetCsl := ""
    cChavePIS  := ""
    cChaveCOF  := ""
    cChaveCSL  := ""
    cChaveInss := ""
    cChaveIss  := ""
    cCIDE      := GetMv("MV_CIDE",,"CIDE")
    nRegSe2	   := SE2->(RecNo())
    cUniao	   := GetMv("MV_UNIAO")
    cForInss   := GetMv("MV_FORINSS")
    aRecnos    := {}
    nLoop      := 0
    nSobra     := 0
    nValorTit  := 0
    nRetOriPIS := 0
    nRetOriCOF := 0
    nRetOriCSL := 0
    nVlMinImp  := GetNewPar("MV_VL10925",5000)
    nVlMinPcc  := GetNewPar("MV_VL13137",10)
    lRestValImp := .F.
    lRetParc   := .T.
    nInss      := SE2->E2_INSS
    cPrefOri   := SE2->E2_PREFIXO
    cNumOri    := SE2->E2_NUM
    cParcOri   := SE2->E2_PARCELA
    cTipoOri   := SE2->E2_TIPO
    cCfOri     := SE2->E2_FORNECE
    cLojaOri   := SE2->E2_LOJA
    lZerouImp  := .F.

    //Controla o Pis Cofins e Csll na baixa
    lPCCBaixa := SuperGetMv("MV_BX10925",.T.,"2") == "1"

    //1-Cria NCC/NDF referente a diferenca de impostos entre emitidos (SE2) e retidos (SE5)
    //2-Nao Cria NCC/NDF, ou seja, controla a diferenca num proximo titulo
    //3-Nao Controla
    cNccRet       := SuperGetMv("MV_NCCRET",.F.,"1")
    lIRPFBaixa    := .F.
	lCalcIssBx    := IsIssBx("P")
	lCIDE  	      := cPaisLoc == "BRA" .And. SuperGetMv("MV_FGCIDE",.T.,"2") == "2" // Define o fato gerador do imposto CIDE. 1 = Baixa ou 2 = Emissão
    cTipoFor 	  := ""
    //Como a variavel cTipoFor eh destinada mais especificamente ao calculo de IRRF (pelo campo A2_IRPROG) utilizar outra para outros impostos como o INSS
    cTipoFor02	  := ""
    nRegSeD       := SED->(RecNo())
    aTpImp        := {}
    aFlagCTB      := {}
    lUsaFlag      := SuperGetMV( "MV_CTBFLAG" , .T. /*lHelp*/, .F. /*cPadrao*/)
    dDataIni      := firstDay( dOldVencRe )
    dDataFim      := LastDay( dOldVencRe )
    cTitPai       := Rtrim(SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)) //E2_TITPAI
    cCodAprov     := SE2->E2_CODAPRO
    cLojaImp      := PadR( "00", TamSX3( "A2_LOJA" )[1], "0" )
    aAreaSed      := {}
    aAreaSa2      := {}
    dVencto       := CTOD("//")
    cKeySe2       := ""
    cFornLoja     := ""
    lVerMinIss	  := .T. //Verifica se o valor calculado para ISS esta dentro do valor minimo.
    cForMinISS	  := GetNewPar("MV_FMINISS","1")
    lGravRegIss	  := .T. //Libera registro de ISS para ser gravado.
    lVcAntIss 	  := (SuperGetMV("MV_ANTVISS",.T.,"2") == "1")  //Antecipa ou nao o vencimento do ISS em caso de vencimento em dia nao util
    lEmpPub		  := IsEmpPub()
    lF050ISS 	  := ExistBlock("F050ISS")
    lF050SES	  := ExistBlock("F050SES")
    lAtuSldNat    := .T.
    lTitReteu     := .F.
    lRatAutPrj	  := Type("LF050AUTO") =="L" .and. lF050Auto .and. Type("aAutoAFR") # "U" .and. !Empty (aAutoAFR)//rateio automatico de projetos
    cForSEST      := PadR( GetMv("MV_FORSEST",,""), Len( SE2->E2_FORNECE ) )
    lRetOutMod	  := F050TitRet()
    cRetIns   	  := M->E2_RETINS
    cTpTaxa       := Iif(SE2->E2_TIPO $ MVPAGANT+"/"+MV_CPNEG, MVTXA, MVTAXA)
    nTotImp       := 0
    lRndSest      := SuperGetMv("MV_RNDSEST",.F.,.F.)
    aForISSCPM    := {}
    aImpos        := aClone(__aVetImp)
    aTitImp       := {}
    nY            := 0
    aRecImpos     := {}
    aImps         := {}
    cNatIrf       := &(GetMv("MV_IRF",.F.,"'IRF'"))
    aDeleta       := {}
    nBaseIrrf     := Iif(__lLocBRA,SE2->E2_BASEIRF,0)
    cSEST         := GetMv("MV_SEST",,"")
    lDiaUIss      := (SuperGetMV("MV_DIAUISS",.T.,0) > 0) 
    bPMSDlgFI	  := {||PmsDlgFI(4,M->E2_PREFIXO,M->E2_NUM,M->E2_PARCELA,M->E2_TIPO,M->E2_FORNECE,M->E2_LOJA,.F.)}

    //Contabilização do Rateio
    cProcPCO		:= "000021"
    cItemPCO		:= "01"
    cRecPag 		:= "P"
    cPadrao 		:= "511"
    lPadrao 		:= verPadrao(cPadrao)
    cSeq			:= ""

    // só criar o array se for IR
    If nOldIRRF > 0 
        aTitImp := ImpCtaPg(cNatIrf)
    EndIf
    // Guarda conteudo do Gera Dirf *
    cDirfAlt := M->E2_DIRF

    // Caso nao exista os tres impostos, o codigo de retencao sera diferenciado para cada imposto
    If cAlias == "SE2"
        If (SE2->E2_PIS > 0 .or. !Empty(SE2->E2_PARCPIS))
            cChavePIS := SE2->(E2_PREFIXO+E2_NUM+E2_PARCPIS) + cTpTaxa
        EndIf

        If (SE2->E2_COFINS > 0 .or. !Empty(SE2->E2_PARCCOF))
            cChaveCOF := SE2->(E2_PREFIXO+E2_NUM+E2_PARCCOF) + cTpTaxa
        EndIf

        If (SE2->E2_CSLL > 0 .or. !Empty(SE2->E2_PARCSLL))
            cChaveCSL	:= SE2->(E2_PREFIXO+E2_NUM+E2_PARCSLL) + cTpTaxa
        EndIf

        If (SE2->E2_INSS > 0 .OR. !Empty(SE2->E2_PARCINS))
            cChaveInss	:= SE2->(E2_PREFIXO+E2_NUM+E2_PARCINS) + cTpTaxa
        EndIf
    EndIf

    If	(SE2->E2_PIS <= 0 .Or. SE2->E2_COFINS <= 0 .Or. SE2->E2_CSLL <= 0 )
        cCodRetPis := If (__lLocBRA .and. !Empty(SE2->E2_CODRPIS),SE2->E2_CODRPIS,"5979")
        cCodRetCof := If (__lLocBRA .and. !Empty(SE2->E2_CODRCOF),SE2->E2_CODRCOF,"5960")
        cCodRetCsl := If (__lLocBRA .and. !Empty(SE2->E2_CODRCSL),SE2->E2_CODRCSL,"5987")
        If __lFNCDRET
            aCRets :=ExecBlock("FINCDRET")
            If aScan(aCRets,cCodRetIr) > 0
                cCodRetPis := cCodRetCof := cCodRetCsl := cCodRetIr
            EndIf
        End
    Else
        // Se os 3 impostos juntos for maior que a media de retencao, o codigo sera o mesmo
        // para os tres.
        If SE2->(E2_PIS+E2_COFINS+E2_CSLL) > ((GetMv("MV_VRETPIS")+GetMv("MV_VRETCOF")+GetMv("MV_VRETCSL")) / 3)
            cCodRetPis := If (__lLocBRA .and. !Empty(SE2->E2_CODRPIS),SE2->E2_CODRPIS,"5952")
            cCodRetCof := If (__lLocBRA .and. !Empty(SE2->E2_CODRCOF),SE2->E2_CODRCOF,"5952")
            cCodRetCsl := If (__lLocBRA .and. !Empty(SE2->E2_CODRCSL),SE2->E2_CODRCSL,"5952")
            If __lFNCDRET
                aCRets :=ExecBlock("FINCDRET")
                If aScan(aCRets,cCodRetIr) > 0
                    cCodRetPis := cCodRetCof := cCodRetCsl := cCodRetIr
                EndIf
            End
        EndIf
    Endif

    If !__lInsMR
        IF SED->ED_DEDINSS == "2"  //Nao desconta o INSS do principal
            nInss := 0
        Endif
    Endif

    SA2->(dbSetOrder(1))
    SA2->(MSSeek(xFilial("SA2")+SE2->(E2_FORNECE+E2_LOJA)))

    lIRPFBaixa := IIf( __lLocBRA, SA2->A2_CALCIRF == "2", .F.)

    // Verifica se esta utilizando multiplas naturezas
    If MV_MULNATP .And. M->E2_MULTNAT == "1" .And.;
        ((SE2->E2_VALOR != nValorAnt 	.Or. SE2->E2_IRRF != nOldIRRF	.Or.;
        SE2->E2_ISS != nOldISSInt	.Or. SE2->E2_INSS != nOldIns .Or.;
        SE2->E2_PIS != nOldPisAnt	.Or. SE2->E2_COFINS != nOldCofAnt .Or. ;
        SE2->E2_CSLL != nOldCslAnt) .Or. Len(aCols) > 0)

        If mv_par06 == 1
            nTotImp := If(lIRPFBaixa, 0, M->E2_IRRF)
            nTotImp += If(lCalcIssBx, 0, M->E2_ISS + If(__lBtrISS,M->E2_BTRISS,0))
            nTotImp += If(lPccBaixa, 0, M->E2_PIS + M->E2_COFINS + M->E2_CSLL )
            nTotImp += M->E2_RETENC + M->E2_SEST + nInss
        Else
            nTotImp := 0
        EndIf

        MultNat("SE2" /*cAlias*/, @nHdlPrv, @nTotal, @cArquivo, (mv_par04 == 1) /*lContabiliza*/, 4 /*nOpc*/,  nTotImp /*nImpostos*/, (mv_par10 = 2 .And. mv_par06 = 2) /*lRatImpostos*/,;
        aHeader /*aCols*/, aCols /*aHeader*/, aRegs /*aRegs*/, .T. /*lGrava*/, .F. /*lMostraTela*/, nil /*lRotAuto*/,;
        lUsaFlag /*lUsaFlag*/, @aFlagCTB /*@aFlagCTB*/ ) // Chama a rotina para distribuir o valor entre as naturezas

        If nTotal > 0
            lDigita := IIF( mv_par01 == 1, .T., .F. )

            If  UsaSeqCor()
                aDiario := {}
                aDiario := {{"SE2",SE2->(recno()),SE2->E2_DIACTB,"E2_NODIA","E2_DIACTB"}}
            Else
                aDiario := {}
            EndIf

            //Envia para Lan‡amento Contábil - Alteracao Multi
            cA100Incl( cArquivo, nHdlPrv, 3/*nOpcx*/,cLote, lDigita, ( mv_par07 == 1 ) /*lAglut*/, /*cOnLine*/, /*dData*/, /*dReproc*/, @aFlagCTB, /*aDadosProva*/, aDiario )
            aFlagCTB := {}//Limpa o coteudo apos a efetivacao do lancamento
        EndIf
    Else
        If lAtuSldNat .And. SE2->E2_FLUXO == 'S'
            // Tiro o valor da natureza antiga
            AtuSldNat(cOldNaturez, dOldVencRe, SE2->E2_MOEDA, If(SE2->E2_TIPO $ MVPAGANT+"/"+MV_CPNEG,"3","2"), "P", nOldSaldo, nOldVlCruz, If(SE2->E2_TIPO $ MVABATIM, "+","-"),,FunName(),"SE2",SE2->(Recno()),4)
            // Somo o valor na nova natureza
            AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, If(SE2->E2_TIPO $ MVPAGANT+"/"+MV_CPNEG,"3","2"), "P", M->E2_VALOR, SE2->E2_VLCRUZ, If(SE2->E2_TIPO $ MVABATIM, "-","+"),,FunName(),"SE2", SE2->(Recno()), 4)
        Endif
        //Se a data do titulo principal foi alterada, os venctos dos abatimentos devem ser alterados
        If dOldVencRe != SE2->E2_VENCTO .OR. dOldVencRe != SE2->E2_VENCREA
            dVencto 	 := SE2->E2_VENCTO
            dVencRea  := SE2->E2_VENCREA
            cKeySe2 	 := SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA)
            cFornLoja := SE2->(E2_FORNECE+E2_LOJA)
            If SE2->(MsSeek(xFilial("SE2")+cKeySE2))
                While SE2->(!Eof()) .And. cKeySe2 == SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA)
                    If SE2->(E2_FORNECE + E2_LOJA ) == cFornLoja .And.;
                    SE2->E2_TIPO $ MVABATIM
                        If lAtuSldNat .And. SE2->E2_FLUXO == 'S'
                            // Tiro o valor da natureza antiga
                            AtuSldNat(cOldNaturez, dOldVencRe, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ,"+",,FunName(),"SE2",SE2->(Recno()),4)
                        Endif
                        Reclock("SE2", .F. )
                        SE2->E2_VENCTO  := dVencto
                        SE2->E2_VENCREA := dVencRea
                        MsUnlock()
                        If lAtuSldNat .And. SE2->E2_FLUXO == 'S'
                            // Somo o valor na nova natureza
                            AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "-",,FunName(),"SE2",SE2->(Recno()),4)
                        Endif
                    EndIf
                    SE2->(dbSkip())
                Enddo
            Endif
            SE2->(MsGoto(nRegSe2))
        Endif
    EndIf

    Reclock("SE2")
    If (SE2->E2_ACRESC != nOldVlAcres)
        Replace E2_SDACRES With E2_ACRESC
    Endif
    If (SE2->E2_DECRESC != nOldVlDecres)
        Replace E2_SDDECRE With E2_DECRESC
    EndIf

    //Permissao para alterar titulos liberados para pagamento
    If GETMV("MV_CTLIPAG")
        lAltLib := (SuperGetMv("MV_ALTLIPG",.F.,"S") == "S")
        //Se nao permite a alteracao verifico a liberacao.
        //Se parametrizado para que titulo alterado volte para a liberacao de pagamentos
        // Limpo a data de liberacao
        If lAltLib .and. !Empty(SE2->E2_DATALIB) .and. SuperGetMv("MV_CANLIPG",.F.,"N") == "S"
            RecLock("SE2")
            SE2->E2_DATALIB := CTOD("//")
            SE2->E2_STATLIB := "01"
            MsUnlock()
        Endif
    Endif

    IF SE2->E2_VALOR != nValorAnt
        Reclock("SE2")
        Replace E2_SALDO With E2_VALOR
        nValForte := ConvMoeda(E2_EMISSAO,E2_VENCTO,E2_VALOR,GetMv("mv_mcusto"))
        // Atualiza saldo do fornecedor.
        dbSelectArea("SA2")
        dbSeek(cFilial+SE2->E2_FORNECE+SE2->E2_LOJA)
        RecLock("SA2")
        If !(SE2->E2_TIPO $ MVABATIM+"/"+MVPAGANT+"/"+MV_CPNEG )
            SA2->A2_SALDUP -=Round(NoRound(xMoeda(nOldValor	 ,SE2->E2_MOEDA,1,SE2->E2_EMISSAO,3),3),2)
            SA2->A2_SALDUPM-=Round(NoRound(xMoeda(nOldValor	 ,SE2->E2_MOEDA,nMoeda,SE2->E2_EMISSAO,3),3),2)
            SA2->A2_SALDUP +=Round(NoRound(xMoeda(SE2->E2_SALDO,SE2->E2_MOEDA,1,SE2->E2_EMISSAO,3),3),2)
            SA2->A2_SALDUPM+=Round(NoRound(xMoeda(SE2->E2_SALDO,SE2->E2_MOEDA,nMoeda,SE2->E2_EMISSAO,3),3),2)
        Else
            SA2->A2_SALDUP +=Round(NoRound(xMoeda(nOldValor	 ,SE2->E2_MOEDA,1,SE2->E2_EMISSAO,3),3),2)
            SA2->A2_SALDUPM+=Round(NoRound(xMoeda(nOldValor	 ,SE2->E2_MOEDA,nMoeda,SE2->E2_EMISSAO,3),3),2)
            SA2->A2_SALDUP -=Round(NoRound(xMoeda(SE2->E2_SALDO,SE2->E2_MOEDA,1,SE2->E2_EMISSAO,3),3),2)
            SA2->A2_SALDUPM-=Round(NoRound(xMoeda(SE2->E2_SALDO,SE2->E2_MOEDA,nMoeda,SE2->E2_EMISSAO,3),3),2)
        EndIf
        nValForte := ConvMoeda(SE2->E2_EMISSAO,SE2->E2_VENCTO,A2_SALDUP,GetMv("mv_mcusto"))
        dbSelectArea("SA2")
        If SA2->A2_SALDUPM > A2_MSALDO
            Replace A2_MSALDO With SA2->A2_SALDUPM
        EndIf
    EndIF

    cTipoFor	:= IIf(SA2->A2_TIPO=="J" .AND. lIRProg == "1","F",SA2->A2_TIPO)
    cTipoFor02	:= IIf(!Empty(SA2->A2_TIPO),SA2->A2_TIPO,"J")

    // Verifica se houve alteracao de VENC.REAL	e nao e tipo imposto 'TX'
    If SE2->E2_VENCREA != dOldVencRe .And. !SE2->E2_TIPO $ "TX /ISS/INS/SES" .and. !lPccBaixa

        dbSelectArea("SE2")
        //IRRF
        If lIRPFBaixa
            dVencRea := F050VImp("IRRF",SE2->E2_EMISSAO,SE2->E2_EMIS1,SE2->E2_VENCREA,,,lIRPFBaixa) // Calcula o vencimento do imposto
        Else
            dVencRea := F050VImp("IRRF",SE2->E2_EMISSAO,SE2->E2_EMIS1,SE2->E2_VENCREA,cCodRetIr,cTipoFor) // Calcula o vencimento do imposto
        Endif
        
            For nY := 1 to Len(aTitImp)
                SE2->(DbGoto(aTitImp[nY][9]))
                If SE2->E2_SALDO > 0
                    Reclock("SE2")
                    SE2->E2_VENCREA 	:= dVencrea
                    SE2->E2_VENCTO 	:= dVencrea
                    MsUnlock()
                Endif
            Next nY   
    
        dbGoto(nRegSe2)
        
        //PCC
        dVencRea := F050VImp("PIS",SE2->E2_EMISSAO,SE2->E2_EMIS1,SE2->E2_VENCREA,cCodRetPis) // Calcula o vencimento do imposto
        //Atualiza data de vencimento do titulo de PIS
        If !Empty(cChavePis) .and. SE2->(MsSeek(cFilial+cChavePis+cUniao))
            If SE2->E2_SALDO > 0
                Reclock("SE2")
                SE2->E2_VENCREA 	:= dVencrea
                SE2->E2_VENCTO 	:= dVencrea
                MsUnlock()
            Endif
        Endif
        //Atualiza data de vencimento do titulo de Cofins
        If !Empty(cChaveCof) .and. SE2->(MsSeek(cFilial+cChaveCof+cUniao))
            If SE2->E2_SALDO > 0
                Reclock("SE2")
                SE2->E2_VENCREA 	:= dVencrea
                SE2->E2_VENCTO 	:= dVencrea
                MsUnlock()
            Endif
        Endif
        //Atualiza data de vencimento do titulo de Cofins
        If !Empty(cChaveCsl) .and. SE2->(MsSeek(cFilial+cChaveCsl+cUniao))
            If SE2->E2_SALDO > 0
                Reclock("SE2")
                SE2->E2_VENCREA := dVencrea
                SE2->E2_VENCTO 	:= dVencrea
                MsUnlock()
            Endif
        Endif
    Endif
    dbGoto(nRegSe2)

    If lSpbInUse
        cModSpb := IIf(Empty(SE2->E2_MODSPB), "1",SE2->E2_MODSPB)
    Endif

    // Verifica se houve alteracao de Irrf
    If SE2->E2_IRRF != nOldIrrf .or. SE2->E2_CODRET<>cOldCodRet
        dbSelectArea("SE2")
        nRegSe2 := RecNo()
        cTipoSE2 := SE2->E2_TIPO
        nValorIr := SE2->E2_IRRF
        If nOldIrrf != 0
            If !__lIrfMR
                If lIRPFBaixa
                    dVencRea := F050VImp("IRRF",SE2->E2_EMISSAO,SE2->E2_EMIS1,SE2->E2_VENCREA,,,lIRPFBaixa) // Calcula o vencimento do imposto
                Else
                    dVencRea := F050VImp("IRRF",SE2->E2_EMISSAO,SE2->E2_EMIS1,SE2->E2_VENCREA,cCodRetIr,cTipoFor) // Calcula o vencimento do imposto
                Endif
            EndIf

            //Deleta os registros na FK3 e FK4
            FinSetAPrc("SE2")
            FinDelEst("SE2",nRegSE2)
            FinSetAPrc("")

            If __lRateioIR
                aEval(aTitImp,{|x| Aadd(aDeleta, x[9])})
                
                //Deleta os impostos na SE2
                If FindFunction("FinDelImp")
                    FinDelImp(aDeleta)
                Endif    

                //Gera titulo de IRRF              
                FGrvIRRF("FINA050",dVencRea,dEmissao,cPrefixo,cNum,Iif(cTipoSE2 $ MVPAGANT+"/"+MV_CPNEG  .And. !lPCCBaixa,MVTXA,MVTAXA),nValorIr,nMoeda,,cGeraDIRF,cCodRetIR,,;
                        ,,nRegSE2,'SE2',lIRPFBaixa,aRecImpos,__oRatIrf:aRatIrf,@aImps )
            Else
                If Len(aTitImp) > 0 
                    
                    SE2->(DbGoto(aTitImp[1][9]))
                    cParcImp := SE2->E2_PARCELA
                    If nValorIr != 0

                        If !__lIrfMR
                            Reclock("SE2")
                            SE2->E2_VALOR := nValorIr
                            SE2->E2_SALDO := nValorIr
                            SE2->E2_VLCRUZ:= Round( nValorIr, MsDecimais(1) )
                            //Trata a alteração do codigo de retenção *
                            SE2->E2_VENCREA 	:= dVencrea
                            SE2->E2_VENCTO 	:= dVencrea
                            SE2->E2_DIRF    := If(cGeraDirf=="2" .and. !Empty(cCodRetIr),"1",cGeraDirf)
                            SE2->E2_CODRET  := cCodRetIr
                            MsUnLock()

                            //Alimentação das matrizes aRecImpos e aImps para gravação
                            //da FK3 e FK4
                            AADD(aRecImpos,{"SE2",Recno()})
                            Aadd(aImps, { "", nBaseIrrf, nValorIr, nBaseIrrf, nValorIr,"", ;
				                {}, "IRF", "1", cNatIrf,"",SE2->(Recno()),"","1", "2", "","","", "2",.T.,0, 0,SA2->A2_CGC})

                        EndIf

                        PCODetLan("000002","06","FINA050")// Altera o lancamento de IRRF gerado no PCO

                        dbGoto(nRegSE2)
                        Reclock("SE2",.F.)
                        SE2->E2_PARCIR := cParcImp
                        msUnLock()
                        
                        aEval(aTitImp,{|x| Aadd(aDeleta, x[9])},2)
                    Else
                        aEval(aTitImp,{|x| Aadd(aDeleta, x[9])})

                        dbGoto(nRegSE2)
                        Reclock("SE2",.F.)
                        SE2->E2_PARCIR := " "
                        msUnLock()
                    EndIf

                    If !__lIrfMR .and. !Empty(aDeleta)
                        //Deleta os impostos na SE2
                        If FindFunction("FinDelImp")
                            FinDelImp(aDeleta)
                        Endif    
                    Endif
                Else
                    nOldIrrf := 0
                EndIf
            Endif    
            dbGoto(nRegSe2)
        Endif

        // Verifica se informado IRRf sem existir
        // anteriormente.
        If nOldIrrf = 0 .And. SE2->E2_IRRF != 0 .and. !lIRPFBaixa
            If !__lIrfMR
                //Gera titulo de IRRF
                dVencRea := F050VImp("IRRF",dEmissao,dEmis1,dVctoReal,cCodRetIr,cTipoFor) // Calcula o vencimento do imposto                
                nValorIr := SE2->E2_IRRF

                FGrvIRRF("FINA050",dVencRea,dEmissao,cPrefixo,cNum,Iif(cTipoSE2 $ MVPAGANT+"/"+MV_CPNEG  .And. !lPCCBaixa,MVTXA,MVTAXA),nValorIr,nMoeda,,cGeraDIRF,cCodRetIR,,;
		                ,,nRegSE2,'SE2',lIRPFBaixa,aRecImpos,If(__lRateioIR,__oRatIrf:aRatIrf,NIL),@aImps )
            Endif
        EndIf
        dbSelectArea("SE2")
        dbGoto(nRegSe2)
    ElseIf !__lIrfMR .And. SE2->E2_IRRF > 0 .and. dOldVencRe <> dVctoReal
        dbSelectArea("SE2")
        nRegSe2 := SE2->(RecNo())
        nValorIr := SE2->E2_IRRF
        If Len(aTitImp) > 0 
            For nY := 1 to Len(aTitImp)
                SE2->(DbGoto(aTitImp[nY][9]))
                If SE2->E2_SALDO == SE2->E2_VALOR
                    dVencRea := F050VImp("IRRF",dEmissao,dEmis1,dVctoReal,cCodRetIr,cTipoFor) // Calcula o vencimento do imposto
                    RecLock("SE2",.F.)
                    SE2->E2_VENCREA := dVencrea
                    SE2->E2_VENCTO 	:= dVencRea
                    SE2->E2_VENCORI	:= dVencRea
                    MsUnlock()
                EndIf
            Next nY   
        EndIf
        dbGoto(nRegSe2)
    EndIf

    If cPaisLoc != "RUS"
        If __lLocBRA
            aAreaSA2 := SA2->( GetArea() )
            SA2->( dbSetOrder(1) ) //A2_FILIAL+A2_COD+A2_LOJA
            If SA2->( msSeek( FWxFilial("SA2") + M->E2_FORNECE + M->E2_LOJA ) )
                aForISSCPM := BtrISSMun( M->E2_CODSERV, SA2->A2_EST, SA2->A2_COD_MUN  )
            EndIf
            RestArea(aAreaSA2)
            FwFreeArray(aAreaSA2)
        EndIf
        // Verifica se houve alteracao do ISS bitributacao
        If Len(aForISSCPM) >= 2 .AND. Len(ISSCPOM("T", M->(E2_FORNECE+E2_LOJA), M->E2_CODSERV)) > 0 //Verifica se existe cadastro na tabela CLI - Bitributação do ISS

            If __lBtrISS .And. !lCalcIssBx .And. (SE2->E2_BTRISS != nBtrISSOri .Or. (SE2->E2_BTRISS == 0 .And. !(SE2->E2_TIPO $ MVISS+"/"+MVTAXA+"/"+MVINSS+"/"+"SES"+"/"+MVPAGANT+"/"+MV_CPNEG)))
                nRegSe2 := SE2->( RECNO() )
                nValorISS := SE2->E2_BTRISS

                If SE2->( msSeek( xFilial("SE2", SE2->E2_FILORIG) + SE2->E2_PREFIXO + SE2->E2_NUM + SE2->E2_PARCISS + MVISS + aForISSCPM[1] + aForISSCPM[2] ) )
                    If nValorISS != 0
                        Reclock("SE2")
                        SE2->E2_VALOR := nValorISS
                        SE2->E2_SALDO := nValorISS
                        SE2->( msUnLock() )
                    Else
                        If !("FINA290" $ SE2->E2_ORIGEM)
                            Iif( __lIntPFS .And. FindFunction("JDelTitCP"), JDelTitCP(SE2->(Recno())), Nil ) // Integração SIGAPFS x SIGAFIN remove os desdobramentos quando o titulo for deletado
                            FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                            Reclock("SE2",.F.,.T.)
                            SE2->( dbDelete() )
                            SE2->( msUnLock() )
                        Endif
                    EndIf
                Else
                    nBtrISSOri := 0
                EndIf
                SE2->( dbGoto(nRegSe2) )
                
            EndIf
        EndIf

        FwFreeArray(aForISSCPM)

        // Verifica se houve alteracao de Iss
        If !lCalcIssBx .and. (SE2->E2_ISS != nOldISSInt .Or. (SE2->E2_ISS == 0 .And. !(SE2->E2_TIPO $ MVISS+"/"+MVTAXA+"/"+MVINSS+"/"+"SES"+"/"+MVPAGANT+"/"+MV_CPNEG)))
            dbSelectArea("SE2")
            nRegSe2  := RecNo()
            nValorISS  := SE2->E2_ISS
            If (dbSeek(cFilial+SE2->E2_PREFIXO+SE2->E2_NUM+SE2->E2_PARCISS+MVISS+SuperGetMV("MV_MUNIC")))
                If nValorISS != 0
                    If !__lIssMR
                        Reclock("SE2")
                        SE2->E2_VALOR := nValorISS
                        SE2->E2_SALDO := nValorISS
                        PCODetLan("000002","09","FINA050")		// Altera o lancamento de ISS gerado no PCO
                    EndIf
                Else
                    If !("FINA290" $ SE2->E2_ORIGEM)
                        PCODetLan("000002","09","FINA050",.T.)	// Apaga o lancamento de ISS gerado no PCO

                        If !__lIssMR
                            FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                            Reclock("SE2",.F.,.T.)
                            dbDelete()
                        EndIf
                    EndIf
                EndIf
            Else
                nOldIssInt := 0
            EndIf
            dbGoto(nRegSe2)
        EndIf

    EndIf

    //Verificar valor minimo do ISS.
    If cPaisLoc $ "BRA" .And. SE2->E2_FRETISS == "2" // Nao verificar minimo do ISS
        lVerMinIss := .F.
    Endif

    If lVerMinIss .and. ;
    ((cForMinISS == "1" .And. SE2->E2_ISS <= SuperGetMv("MV_VRETISS",.F., 0)) .Or. ;
    (cForMinISS == "2" .And. SE2->E2_BASEISS <= GetNewPar("MV_VBASISS",0)))
        lGravRegIss	:=	.F. //Nao libero registro de acordo com o valor minimo do ISS.
    EndIf

    // Verifica se informado ISS sem existir anteriormente.
    If lGravRegIss .AND. !lCalcIssBx 
        If !__lIssMR .and. !lDiaUIss
            //Calculo da data de vencimento do ISS
            If (SE2->E2_VENCREA != dOldVencRe) .OR. (SE2->E2_NATUREZ != cOldNatPFS) .or. (nOldIssInt == 0 .And. SE2->E2_ISS != 0)
			    dVenISS  := F050VIMP("ISS", dEmissao, dEmis1, dVencto, /*cCodRet*/, /*cTipoPes*/, .T.)
                dVencRea :=	DataValida(dVenISS,IIF(lVcAntIss,.F.,.T.))
                dVenISS	 :=	IIF(dVenIss > dVencRea, dVencRea, dVenIss)
            EndIf
        Endif
        If	(nOldIssInt == 0 .And. SE2->E2_ISS != 0) //Criando o registo do ISS
            If !__lIssMR
				nValorIss := SE2->E2_ISS
                //Gera titulo de ISS Cria o fornecedor, caso nao exista
                dbSelectArea("SA2")
                If !(dbSeek(cFilial+GetMV("MV_MUNIC")))
                    Reclock("SA2",.T.)
                    Replace A2_FILIAL With cFilial
                    Replace A2_COD	  With GetMV("MV_MUNIC")
                    Replace A2_LOJA   With cLojaImp
                    Replace A2_NOME   With STR0029  // "MUNICIPIO"
                    Replace A2_NREDUZ With STR0029  // "MUNICIPIO"
                    Replace A2_BAIRRO With "."
                    Replace A2_MUN	  With "."
                    Replace A2_EST	  With SuperGetMv("MV_ESTADO")
                    Replace A2_END	  With "."
                EndIf

                cParcISS := STRZERO(1,nTamParc)

                While ( .T. )
                    //Verifica se ja' ha' titulo de ISS com esta numeração
                    dbSelectArea("SE2")
                    If (dbSeek(cFilial+cPrefixo+cNum+cParcISS+"ISS"+GetMV("MV_MUNIC")))
                        cParcISS := Soma1( cParcISS,,.t. )
                        Loop
                    EndIf
                    Exit
                Enddo

                RecLock("SE2",.T.)
                SE2->E2_FILIAL  := cFilial
                SE2->E2_PREFIXO := cPrefixo
                SE2->E2_NUM	  	 := cNum
                SE2->E2_PARCELA := cParcIss
                SE2->E2_NATUREZ := &(GetMv("MV_ISS"))
                SE2->E2_TIPO	 := MVISS
                SE2->E2_EMISSAO := dEmissao
                SE2->E2_VALOR   := nValorIss
                SE2->E2_VENCTO  := dVenISS
                SE2->E2_SALDO   := nValorIss
                SE2->E2_VENCREA := dVencRea
                SE2->E2_VENCORI := dVenISS
                SE2->E2_FORNECE := GetMV("MV_MUNIC")
                SE2->E2_LOJA    := cLojaImp
                SE2->E2_NOMFOR  := SA2->A2_NREDUZ
                SE2->E2_MOEDA   := 1
                SE2->E2_VLCRUZ 	:= Round( nValorIss, MsDecimais(1) )
                SE2->E2_ORIGEM 	:= "FINA050"
                SE2->E2_EMIS1 	:= dDataBase
                //Grava campo E2_TITPAI
                SE2->E2_TITPAI   := cTitPai
                SE2->E2_CODAPRO  := cCodAprov

                If lSpbInUse
                    Replace	SE2->E2_MODSPB with cModSpb
                Endif

                SE2->E2_FILORIG  := If(Empty(SE2->E2_FILORIG),cFilAnt,SE2->E2_FILORIG)
            EndIf
            // Ponto de Entrada para Titulo ISS
            If lF050ISS
                Execblock("F050ISS",.F.,.F.,nRegSE2)
            Endif

            // Grava o lancamento de ISS no PCO
            PCODetLan("000002","09","FINA050")

            If !__lIssMR
                //Cria a natureza ISS caso nao exista
                dbSelectArea("SED")
                cVar := Alltrim(&(GetMv("MV_ISS")))
                cVar := cVar + Space(10-Len(cVar))
                If !(dbSeek( xFilial("SED") + cVar ) )
                    RecLock("SED",.T.)
                    ED_FILIAL  := xFilial("SED")
                    ED_CODIGO  := cVar
                    ED_CALCIRF := "N"
                    ED_CALCISS := "N"
                    ED_CALCINS := "N"
                    ED_CALCCSL := "N"
                    ED_CALCCOF := "N"
                    ED_CALCPIS := "N"
                    ED_DESCRIC := STR0030			  // "IMPOSTO SOBRE SERVICOS"
                    ED_TIPO	   := "2"
                    MsUnlock()
                EndIf

                //Grava parcela do Iss na parcela do titulo
                dbSelectArea( "SE2" )
                dbGoto( nRegSe2 )
                Reclock( "SE2" , .F. )
                SE2->E2_PARCISS := cParcISS
                MsUnlock()
            EndIf
        ElseIf !__lIssMR .And. lAltera .And. nISSOri != SE2->E2_ISS
            nRegSe2 := SE2->(Recno())
            nValorIss 	:= SE2->E2_ISS
            cChaveIss	:= xFilial("SE2") + SE2->E2_PREFIXO + SE2->E2_NUM + SE2->E2_PARCISS + MVISS + Padr(GetMv("MV_MUNIC"),TamSx3("E2_FORNECE")[1])
            If !Empty(cChaveIss) .and. SE2->(MsSeek(cChaveIss))
                RecLock("SE2",.F.)
                SE2->E2_VALOR	:= nValorIss
                SE2->E2_SALDO	:= nValorIss
                SE2->E2_VLCRUZ 	:= Round( nValorIss, MsDecimais(1) )
                MsUnlock()
            EndIf
            SE2->(DbGoto( nRegSe2 ))
        ElseIf !__lIssMR .And. __lBtrISS .and. lAltera .and. nBtrISSOri <> SE2->E2_BTRISS
            nRegSe2		:= SE2->(Recno())
            nValorIss 	:= SE2->E2_BTRISS
            cChaveIss	:= xFilial("SE2") + SE2->E2_PREFIXO + SE2->E2_NUM + SE2->E2_PARCISS + MVISS + Padr(Upper(SM0->M0_CIDENT),TamSx3("E2_FORNECE")[1])
            If !Empty(cChaveIss) .and. SE2->(MsSeek(cChaveIss))
                RecLock("SE2",.F.)
                SE2->E2_VALOR	:= nValorIss
                SE2->E2_SALDO	:= nValorIss
                SE2->E2_VLCRUZ 	:= Round( nValorIss, MsDecimais(1) )
                MsUnlock()
            Endif
            SE2->(DbGoto( nRegSe2 ))

        ElseIF !lDiaUIss
            If !__lIssMR .and. SE2->E2_VENCTO <> dVenISS .OR. SE2->E2_VENCREA <> dVencRea //Alterando registro caso as datas de vencimentos sejam alteradas.
                nRegSe2 := SE2->(Recno())
                nValorIss 	:= SE2->E2_ISS
                cChaveIss	:= xFilial("SE2") + SE2->E2_PREFIXO + SE2->E2_NUM + SE2->E2_PARCISS + MVISS + Padr(GetMv("MV_MUNIC"),TamSx3("E2_FORNECE")[1])
                If SE2->E2_VENCREA != dOldVencRe .and. !Empty(cChaveIss) .and.;
                    SE2->(MsSeek(cChaveIss)) .and. Empty(SE2->E2_NUMBOR)
                    If SE2->E2_SALDO == nValorIss
                        RecLock("SE2", .F. )
                        SE2->E2_VENCREA := dVencRea
                        SE2->E2_VENCTO	:= dVenIss
                        MsUnlock()
                    Endif
                Endif
                SE2->(DbGoto( nRegSe2 ))
            Endif    
        EndIf

    EndIf
    //Verifica se altero o RETINSS
    If !__lInsMR
        dbSelectArea("SE2")
        nRegSe2 := SE2->(Recno())
        If SE2->(dbSeek(cFilial+SE2->E2_PREFIXO+SE2->E2_NUM+SE2->E2_PARCINS+"INS"+GetMv("MV_FORINSS")))
            If SE2->E2_RETINS <> cRetIns
                Reclock("SE2",.F.)
                SE2->E2_RETINS:= M->E2_RETINS
                SE2->(MsUnlock())
            EndIF

            SE2->(DbGoto( nRegSe2 ))
        EndIF
    EndIf
    // Verifica se houve alteracao de Inss
    dbSelectArea("SE2")
    SE2->(DbGoto( nRegSe2 ))
    If SE2->E2_INSS != nOldIns .OR. (SE2->E2_INSS == 0 .AND. !(SE2->E2_TIPO $ MVISS+"/"+MVTAXA+"/"+MVINSS+"/"+"SES"+"/"+MVPAGANT+"/"+MV_CPNEG)) .or. ;
        ( (SE2->E2_CODRET<>cOldCodRet) .and. cTipoFor02 == 'F')

        nValInss:= SE2->E2_INSS
        If nOldIns != 0
            If (dbSeek(cFilial+SE2->E2_PREFIXO+SE2->E2_NUM+SE2->E2_PARCINS+"INS"+GetMv("MV_FORINSS")))
                If nValInss != 0
                    If !__lInsMR
                        Reclock("SE2")
                        SE2->E2_VALOR := nValInss
                        SE2->E2_SALDO := nValInss
                        SE2->E2_VLCRUZ:= Round( nValInss, MsDecimais(1) )
                        SE2->E2_DIRF    := cGeraDirf
                        SE2->E2_CODRET  := cCodRetIr
                    EndIf

                    PCODetLan("000002","07","FINA050")		// Altera o lancamento de INSS gerado no PCO
                Else
                    PCODetLan("000002","07","FINA050",.T.)	// Apaga o lancamento de INSS gerado no PCO

                    If !__lInsMR
                        //apaga o registro de inss que tornou indevido após alteracao do principal
                        FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                        Reclock("SE2",.F.,.T.)
                        dbDelete()
                        MsUnlock()

                        SE2->(dbGoto(nRegSE2))
                        Reclock("SE2",.F.)
                        SE2->E2_PARCINS := " "
                        MsUnlock()
                        aRecSE2 := FImpExcTit("SE2",SE2->E2_PREFIXO,SE2->E2_NUM,SE2->E2_PARCELA,SE2->E2_TIPO,SE2->E2_FORNECE,SE2->E2_LOJA,@aTpImp)

                        //atualiza os acumulados do fornecedor, de forma que retorne o status de retencao do INSS
                        For nX := 1 to Len(aRecSE2)
                            SE2->(MSGoto(aRecSE2[nX]))
                            FaAvalSE2(4,,,,,,,,,,"INS")
                        Next

                        SE2->(dbGoto(nRegSE2))
                        FImpExcSFQ("SE2",SE2->E2_PREFIXO,SE2->E2_NUM,SE2->E2_PARCELA,SE2->E2_TIPO,SE2->E2_FORNECE,SE2->E2_LOJA)
                    EndIf
                EndIf
            Else
                nOldIns := 0
            Endif
            SE2->(dbGoto(nRegSe2))
        EndIf

        //Verifica se informado INSS sem existir anteriormente.
        If nOldIns = 0 .And. SE2->E2_INSS != 0
            If !__lInsMR
                nValInss := SE2->E2_INSS
                // Gera titulo de INSS
                //Gera titulo de INSS
                FGrvINSS(cPrefixo,cNum,cParcInss,"","",1,nRegSED,nRegSE2,dEmissao,dDataBase,dVctoReal,nValInss,.F.,{},{},cGeraDirf,cCodRetIr,lSpbInUse,cModSpb)
            EndIf
            // Grava o lancamento de INSS no PCO
            PCODetLan("000002","07","FINA050")
        ElseIf !__lInsMR .And. nOldIns <> 0 .And. nOldIns <> SE2->E2_INSS
            SE2->E2_PRETINS := " " // PRET = " " - Retido nele mesmo.
        EndIf
        dbSelectArea("SE2")
        dbGoto(nRegSe2)
    Endif

    If !__lInsMR .And. (SE2->E2_INSS > 0) .and. (dOldVencRe <> dVctoReal)
        //Atualiza data de vencimento do titulo de INSS
        dVencRea := F050VImp("INSS",SE2->E2_EMISSAO,SE2->E2_EMIS1,SE2->E2_VENCREA,,cTipoFor02) // Calcula o vencimento do imposto

        If !Empty(cChaveInss) .AND. SE2->(MsSeek(cFilial + cChaveInss + cForInss))
            If SE2->E2_SALDO > 0
                Reclock("SE2")
                SE2->E2_VENCREA	:= dVencrea
                SE2->E2_VENCTO	:= dVencrea
                //	SE2->E2_EMIS1	:= dVencrea -> Data da contabilização dos impostos não pode ser alterada!
                MsUnlock()
            EndIf
        EndIf
        SE2->(dbGoto(nRegSe2))
    Endif

    // Verifica se houve alteracao de SEST
    If __lLocBRA .And. (SE2->E2_SEST != nOldSES .OR. (SE2->E2_SEST == 0 .And. !(SE2->E2_TIPO $ MVISS+"/"+MVTAXA+"/"+MVINSS+"/"+"SES"+"/"+MVPAGANT+"/"+MV_CPNEG)))
        dbSelectArea("SE2")
        nRegSe2 := RecNo()
        nValSEST:= SE2->E2_SEST
        If nOldSES != 0
            If (dbSeek(cFilial+SE2->E2_PREFIXO+SE2->E2_NUM+SE2->E2_PARCSES+"SES"+AllTrim(cForSEST) ))
                If nValSEST != 0
                    If !__lSestMR
                        Reclock("SE2")
                        SE2->E2_VALOR := nValSEST
                        SE2->E2_SALDO := nValSEST
                        SE2->E2_VLCRUZ:= Round( nValSEST, MsDecimais(1) )
                    EndIf

                    PCODetLan("000002","08","FINA050")		// Altera o lancamento de SEST/SENAT gerado no PCO
                Else
                    PCODetLan("000002","08","FINA050",.T.)	// Apaga o lancamento de SEST/SENAT gerado no PCO
                    If !__lSestMR
                        FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                        Reclock("SE2",.F.,.T.)
                        dbDelete()
                        msUnLock()
                        dbGoto (nRegSE2)
                        Reclock("SE2",.F.)
                        SE2->E2_PARCSES := " "
                    EndIf
                EndIf
            Else
                nOldSES := 0
            EndIf
            dbGoto(nRegSe2)
        EndIf
        // Verifica se informado SEST sem existir
        // anteriormente.
        If nOldSES = 0 .And. SE2->E2_SEST != 0
            nValSEST := SE2->E2_SEST

            If !__lSestMR
                //Gera titulo de SEST Cria o fornecedor, caso nao exista
                dbSelectArea("SA2")
                If !(dbSeek(cFilial+AllTrim(cForSEST) ))
                    Reclock("SA2",.T.)
                    Replace A2_FILIAL With cFilial
                    Replace A2_COD    With GetmV("MV_FORSEST")
                    Replace A2_NOME	With   STR0109 //"Servico Social do Transporte"
                    Replace A2_NREDUZ With  "SEST"
                    Replace A2_LOJA	With cLojaImp
                    Replace A2_MUN 	With "."
                    Replace A2_EST 	With SuperGetMv("MV_ESTADO")
                    Replace A2_BAIRRO With "."
                    Replace A2_END 	With "."
                EndIF

                dNextMes := Month(SE2->E2_EMISSAO)+1
                dNextVen := CTOD("02/"+IIF(dNextMes==13,"01",StrZero(dNextMes,2))+"/"+;
                Substr(Str(IIF(dNextMes==13,Year(SE2->E2_EMISSAO)+1,Year(SE2->E2_EMISSAO))),2),"ddmmyy")
                dVencRea := DataValida(dNextVen,.T.)

                // Verifica parcela do SEST caso exista titulo de SEST com o mesmo numero
                cParcSEST := STRZERO(1,nTamParc)
                DbSelectArea("SE2")
                DbSetOrder(1)

                While .T.
                    // VerIfica se ja' ha' titulo de SEST com esta numera‡„o
                    If (DbSeek(cFilial+cPrefixo+cNum+cParcSEST+"SES"+PadR(cForSEST,6)))
                        cParcSEST := Soma1( cParcSEST,,.t.)
                        Loop
                    EndIf
                    Exit
                Enddo

                //Grava a parcela do SEST no titulo pai fazendo a amarracao titulo x titulo SEST
                dbGoto(nRegSe2)
                RecLock("SE2")
                SE2->E2_PARCSES 	:= cParcSEST

                // Grava titulo de SEST caso n„o exista anterior.
                RecLock("SE2",.T.)
                SE2->E2_FILIAL		:= cFilial
                SE2->E2_PREFIXO 	:= cPrefixo
                SE2->E2_NUM			:= cNum
                SE2->E2_PARCELA 	:= cParcSEST
                SE2->E2_NATUREZ 	:= AllTrim(cSEST)
                SE2->E2_TIPO 		:= "SES"
                SE2->E2_EMISSAO 	:= dEmissao
                SE2->E2_VALOR		:= nValSEST
                SE2->E2_VENCREA 	:= dVencrea
                SE2->E2_SALDO		:= nValSEST
                SE2->E2_VENCTO		:= dVencRea
                SE2->E2_VENCORI 	:= dVencRea
                SE2->E2_EMIS1		:= IIf(Type("dDataEmis1") # "U",IIf(!Empty(dDataEmis1),dDataEmis1,dDataBase),dDataBase)
                SE2->E2_FORNECE	    := Padr(cForSEST,6)
                SE2->E2_LOJA 		:= cLojaImp
                SE2->E2_NOMFOR		:= SA2->A2_NREDUZ
                SE2->E2_MOEDA		:= 1
                SE2->E2_VLCRUZ		:= Iif(lRndSest,Round( nValSEST, MsDecimais(1) ),NoRound( nValSEST, MsDecimais(1) ))
                //Grava campo E2_TITPAI
                SE2->E2_TITPAI   := cTitPai
                SE2->E2_CODAPRO  := cCodAprov

                If lSpbInUse
                    Replace	SE2->E2_MODSPB with cModSpb
                Endif

                SE2->E2_FILORIG  := If(Empty(SE2->E2_FILORIG),cFilAnt,SE2->E2_FILORIG)
            EndIf

            // Ponto de Entrada para Titulo SEST
            If lF050SES
                Execblock("F050SES",.F.,.F.,nRegSE2)
            Endif

            // Grava o lancamento de SEST/SENAT no PCO
            PCODetLan("000002","08","FINA050")

            If !__lSestMR
                //Cria a natureza SEST caso nao exista
                dbSelectArea("SED")
                cVar := Alltrim(cSEST)
                cVar := cVar + Space(10-Len(cVar))
                If !(dbSeek(cFilial+cVar))
                    RecLock("SED",.T.)
                    Replace 	ED_FILIAL  With cFilial,;
                    ED_CODIGO  With cVar	,	;
                    ED_CALCIRF With "N" 	,	;
                    ED_CALCISS With "N"	, 	;
                    ED_CALCINS With "N"	,	;
                    ED_CALCCSL With "N"  ,	;
                    ED_CALCCOF With "N"  ,  ;
                    ED_CALCPIS With "N"  ,	;
                    ED_DESCRIC With STR0109 ,;  // "Servico Social do Transporte"
                    ED_TIPO	   With "2"
                EndIf

                Iif(__lIntPFS .and. FindFunction("JurCompSED"), JurCompSED(SED->(Recno())), Nil) //Integração SIGAPFS - Complemento da Natureza
            EndIf
        Endif
        dbSelectArea("SE2")
        dbGoto(nRegSe2)
    EndIf

    // Verifica se houve alteracao de CIDE
    If !__lCidMR .And. lCIDE
        If SE2->E2_CIDE != nOldCID .OR. ;
        (SE2->E2_CIDE == 0 .And. !(SE2->E2_TIPO $ MVISS+"/"+MVTAXA+"/"+MVINSS+"/"+"CID"+"/"+MVPAGANT+"/"+MV_CPNEG))
            dbSelectArea("SE2")
            nRegSe2  := RecNo()
            nValCIDE:= SE2->E2_CIDE
            If nOldCID != 0
                If (dbSeek(cFilial+SE2->E2_PREFIXO+SE2->E2_NUM+SE2->E2_PARCCID+"CID"+GetMv("MV_FORCIDE")))
                    If nValCIDE != 0
                        Reclock("SE2")
                        SE2->E2_VALOR := nValCIDE
                        SE2->E2_SALDO := nValCIDE
                        SE2->E2_VLCRUZ:= Round( nValCIDE, MsDecimais(1) )
                        msUnLock()
                    Else
                        Iif(__lIntPFS .and. FindFunction("JDelTitCP"), JDelTitCP(SE2->(Recno())), Nil) // Integração SIGAPFS x SIGAFIN remove os desdobramentos quando o titulo for deletado
                        FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                        Reclock("SE2",.F.,.T.)
                        dbDelete()
                        msUnLock()
                        dbGoto (nRegSE2)
                        Reclock("SE2",.F.)
                        SE2->E2_PARCCID := " "
                        msUnLock()
                    EndIf
                Else
                    nOldCID := 0
                EndIf
                dbGoto(nRegSe2)
            EndIf

            // Verifica se informado CIDE sem existir
            // anteriormente.
            If nOldCID = 0 .And. SE2->E2_CIDE != 0
                nValCIDE := SE2->E2_CIDE
                // Gera titulo de CIDE
                // Cria o fornecedor, caso nao exista
                dbSelectArea("SA2")
                If !(dbSeek(cFilial+GetMv("MV_FORCIDE")))
                    Reclock("SA2",.T.)
                    Replace A2_FILIAL With cFilial
                    Replace A2_COD    With GetmV("MV_FORCIDE")
                    Replace A2_NOME	With  "CIDE"
                    Replace A2_NREDUZ With  "CIDE"
                    Replace A2_LOJA	With cLojaImp
                    Replace A2_MUN 	With "."
                    Replace A2_EST 	With SuperGetMv("MV_ESTADO")
                    Replace A2_BAIRRO With "."
                    Replace A2_END 	With "."
                EndIf
                dNextMes := Month(SE2->E2_EMISSAO)+1
                dNextVen := CTOD("02/"+IIF(dNextMes==13,"01",StrZero(dNextMes,2))+"/"+;
                Substr(Str(IIF(dNextMes==13,Year(SE2->E2_EMISSAO)+1,Year(SE2->E2_EMISSAO))),2),"ddmmyy")
                dVencRea := DataValida(dNextVen,.T.)

                // Verifica parcela do CIDE caso exista titulo
                // de CIDE com o mesmo numero.
                cParcCIDE := STRZERO(1,nTamParc)
                DbSelectArea("SE2")
                DbSetOrder(1)
                While .T.
                    // VerIfica se ja' ha' titulo de CIDE com esta numera‡„o
                    If (DbSeek(cFilial+cPrefixo+cNum+cParcCIDE+"CID"+PadR(GetMv("MV_FORCIDE"),6)))
                        cParcCIDE := Soma1( cParcCIDE,,.t.)
                        Loop
                    EndIf
                    Exit
                Enddo
                // Grava a parcela do CIDE no titulo pai fazendo
                // a amarracao titulo x titulo CIDE
                dbGoto(nRegSe2)
                RecLock("SE2")
                SE2->E2_PARCCID 	:= cParcCIDE

                If ( Empty(cCIDE), cCIDE := "CIDE", Nil )

                // Grava titulo de CIDE caso n„o exista anterior.
                RecLock("SE2",.T.)
                SE2->E2_FILIAL		:= cFilial
                SE2->E2_PREFIXO 	:= cPrefixo
                SE2->E2_NUM			:= cNum
                SE2->E2_PARCELA 	:= cParcCIDE
                SE2->E2_NATUREZ 	:= AllTrim(cCIDE)
                SE2->E2_TIPO 		:= "CID"
                SE2->E2_EMISSAO 	:= dEmissao
                SE2->E2_VALOR		:= nValCIDE
                SE2->E2_VENCREA 	:= dVencrea
                SE2->E2_SALDO		:= nValCIDE
                SE2->E2_VENCTO		:= dVencRea
                SE2->E2_VENCORI 	:= dVencRea
                SE2->E2_EMIS1		:= IIf(Type("dDataEmis1") # "U",IIf(!Empty(dDataEmis1),dDataEmis1,dDataBase),dDataBase)
                SE2->E2_FORNECE   := GetMv("MV_FORCIDE")
                SE2->E2_LOJA 		:= cLojaImp
                SE2->E2_NOMFOR		:= SA2->A2_NREDUZ
                SE2->E2_MOEDA		:= 1
                SE2->E2_VLCRUZ		:= Round( nValCIDE, MsDecimais(1) )
                //Grava campo E2_TITPAI
                SE2->E2_TITPAI   := cTitPai
                SE2->E2_CODAPRO  := cCodAprov
                If lSpbInUse
                    Replace	SE2->E2_MODSPB with cModSpb
                Endif

                SE2->E2_FILORIG  := If(Empty(SE2->E2_FILORIG),cFilAnt,SE2->E2_FILORIG)

                // Cria a natureza CIDE caso nao exista
                dbSelectArea("SED")
                cVar := Alltrim(cCIDE)
                cVar := cVar + Space(10-Len(cVar))
                If !(dbSeek( cFilial + cVar ) )
                    RecLock("SED",.T.)
                    Replace 	ED_FILIAL  With cFilial,;
                    ED_CODIGO  With cVar	,	;
                    ED_CALCIRF With "N" 	,	;
                    ED_CALCISS With "N"	, 	;
                    ED_CALCINS With "N"	,	;
                    ED_CALCCSL With "N"  ,	;
                    ED_CALCCOF With "N"  ,  ;
                    ED_CALCPIS With "N"  ,	;
                    ED_DESCRIC With "CIDE", ;
                    ED_TIPO	   With "2"
                    msUnLock()

                    Iif(__lIntPFS .and. FindFunction("JurCompSED"), JurCompSED(SED->(Recno())), Nil) //Integração SIGAPFS - Complemento da Natureza
                EndIf
            EndIf
            dbSelectArea( "SE2" )
            dbGoto( nRegSe2 )
        EndIf
    EndIf

    If !__lPccMR .and. (lAlterNat .or. lAltValor) .and. !lPccBaixa
        Do Case
            Case cModRetPIS == "1"

            nValorTit := SE2->(E2_VALOR+E2_PIS+E2_COFINS+E2_CSLL+E2_IRRF+E2_INSS+E2_ISS)+ ;
            Iif(__lLocBRA, SE2->E2_SEST, 0)

            If (aDadosRet[ 1 ] + nValorTit	> nVlMinImp) .Or. M->(E2_PIS+E2_COFINS+E2_CSLL) > nVlMinPcc 
                lRetParc := .T.
                // Guarda os valores originais
                nRetOriPIS := nVlRetPis
                nRetOriCOF := nVlRetCOF
                nRetOriCSL := nVlRetCSL

                nVlRetPIS := M->E2_PIS
                nVlRetCOF := M->E2_COFINS
                nVlRetCSL := M->E2_CSLL

                nSobra := nDifPCC

                //Havia uma NDF gerada anteriormente e agora deve ser deletada
                If nRecnoNdf > 0
                    nSavRec := SE2->( Recno() )
                    SE2->(dbGoTo(nRecnoNdf))

                    Iif(__lIntPFS .and. FindFunction("JDelTitCP"), JDelTitCP(SE2->(Recno())), Nil) // Integração SIGAPFS x SIGAFIN remove os desdobramentos quando o titulo for deletado
                    FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                    SE2->(RecLock("SE2"))
                    SE2->(dbDelete())
                    SE2->(MsUnlock())

                    SE2->(Dbgoto(nSavRec))
                Endif
                If nSobra < 0 .and. cNCCRet == "1"
                    //Gero NDF com a diferenca
                    ADupCredRt(Abs(nDifPcc),"501",SE2->E2_MOEDA)
                EndIF

                lRestValImp := .T.

                nSavRec := SE2->( Recno() )

                // Exclui a Marca de "pendente recolhimento" dos demais registros
                If aDadosRet[1] > 0
                    aRecnos := aClone( aDadosRet[ 5 ] )

                    cPrefOri  := SE2->E2_PREFIXO
                    cNumOri   := SE2->E2_NUM
                    cParcOri  := SE2->E2_PARCELA
                    cTipoOri  := SE2->E2_TIPO
                    cCfOri    := SE2->E2_FORNECE
                    cLojaOri  := SE2->E2_LOJA

                    For nLoop := 1 to Len( aRecnos )

                        SE2->( dbGoto( aRecnos[ nLoop ] ) )

                        Reclock("SE2",.F.)

                        SE2->E2_PRETPIS := "2"
                        SE2->E2_PRETCOF := "2"
                        SE2->E2_PRETCSL := "2"

                        SE2->( MsUnlock() )

                        If nSavRec <> aRecnos[ nLoop ]
                            dbSelectArea("SFQ")
                            RecLock("SFQ",.T.)
                            SFQ->FQ_FILIAL  := xFilial("SFQ")
                            SFQ->FQ_ENTORI  := "SE2"
                            SFQ->FQ_PREFORI := cPrefOri
                            SFQ->FQ_NUMORI  := cNumOri
                            SFQ->FQ_PARCORI := cParcOri
                            SFQ->FQ_TIPOORI := cTipoOri
                            SFQ->FQ_CFORI   := cCfOri
                            SFQ->FQ_LOJAORI := cLojaOri

                            SFQ->FQ_ENTDES  := "SE2"
                            SFQ->FQ_PREFDES := SE2->E2_PREFIXO
                            SFQ->FQ_NUMDES  := SE2->E2_NUM
                            SFQ->FQ_PARCDES := SE2->E2_PARCELA
                            SFQ->FQ_TIPODES := SE2->E2_TIPO
                            SFQ->FQ_CFDES   := SE2->E2_FORNECE
                            SFQ->FQ_LOJADES := SE2->E2_LOJA
                            MsUnlock()
                        EndIf
                    Next nLoop
                EndIf
                // Retorna do ponteiro do SE2 para a parcela
                SE2->( MsGoto( nSavRec ) )
                Reclock("SE2", .F. )

            Else
                // Grava a Marca de "pendente recolhimento" dos demais registros
                nRetOriPIS := nVlRetPis
                nRetOriCOF := nVlRetCOF
                nRetOriCSL := nVlRetCSL

                Reclock("SE2", .F. )
                SE2->E2_VRETPIS := 0
                SE2->E2_VRETCOF := 0
                SE2->E2_VRETCSL := 0

                If ( !Empty( nRetOriPis ) .Or. !Empty( nRetOriCof ) .Or. !Empty( nRetOriCsl ) )
                    SE2->E2_PRETPIS := "1"
                    SE2->E2_PRETCOF := "1"
                    SE2->E2_PRETCSL := "1"
                EndIf
                MsUnlock()
                lRetParc := .F.
                lRestValImp := .T.
            EndIf

            Case cModRetPIS == "2"
            // Efetua a retencao
            nSavRec := SE2->( Recno() )

            // Exclui a Marca de "pendente recolhimento" dos demais registros
            If aDadosRet[1] > 0
                aRecnos := aClone( aDadosRet[ 5 ] )

                cPrefOri  := SE2->E2_PREFIXO
                cNumOri   := SE2->E2_NUM
                cParcOri  := SE2->E2_PARCELA
                cTipoOri  := SE2->E2_TIPO
                cCfOri    := SE2->E2_FORNECE
                cLojaOri  := SE2->E2_LOJA

                For nLoop := 1 to Len( aRecnos )

                    SE2->( dbGoto( aRecnos[ nLoop ] ) )

                    RecLock("SE2", .F. )

                    SE2->E2_PRETPIS := "2"
                    SE2->E2_PRETCOF := "2"
                    SE2->E2_PRETCSL := "2"

                    SE2->( MsUnlock() )

                    If nSavRec <> aRecnos[ nLoop ]
                        dbSelectArea("SFQ")
                        RecLock("SFQ",.T.)
                        SFQ->FQ_FILIAL  := xFilial("SFQ")
                        SFQ->FQ_ENTORI  := "SE2"
                        SFQ->FQ_PREFORI := cPrefOri
                        SFQ->FQ_NUMORI  := cNumOri
                        SFQ->FQ_PARCORI := cParcOri
                        SFQ->FQ_TIPOORI := cTipoOri
                        SFQ->FQ_CFORI   := cCfOri
                        SFQ->FQ_LOJAORI := cLojaOri

                        SFQ->FQ_ENTDES  := "SE2"
                        SFQ->FQ_PREFDES := SE2->E2_PREFIXO
                        SFQ->FQ_NUMDES  := SE2->E2_NUM
                        SFQ->FQ_PARCDES := SE2->E2_PARCELA
                        SFQ->FQ_TIPODES := SE2->E2_TIPO
                        SFQ->FQ_CFDES   := SE2->E2_FORNECE
                        SFQ->FQ_LOJADES := SE2->E2_LOJA
                        MsUnlock()
                    Endif
                Next nLoop
            Endif
            // Retorna do ponteiro do SE1 para a parcela
            SE2->( MsGoto( nSavRec ) )
            Reclock("SE2", .F. )

            lRetParc := .T.
            Case cModRetPIS == "3"
            // Nao efetua a retencao
            lRetParc := .F.
            lRestValImp := .T.
            // Grava a Marca de "pendente recolhimento" dos demais registros
            nRetOriPIS := nVlRetPis
            nRetOriCOF := nVlRetCOF
            nRetOriCSL := nVlRetCSL

            If ( !Empty( nRetOriPis ) .Or. !Empty( nRetOriCof ) .Or. !Empty( nRetOriCsl ) )
                Reclock("SE2", .F. )
                SE2->E2_PRETPIS := "1"
                SE2->E2_PRETCOF := "1"
                SE2->E2_PRETCSL := "1"
                Reclock("SE2", .F. )
            EndIf
        EndCase
    Else
        lRetParc := .T.
    EndIf

    If !__lPccMR .and. !lPccBaixa

        SE2->( MsGoto( nRegSE2 ) )
        Reclock("SE2" , .F. )

        If lRetParc
            // Grava os campos de valor retido
            SE2->E2_VRETPIS := SE2->E2_PIS
            SE2->E2_VRETCOF := SE2->E2_COFINS
            SE2->E2_VRETCSL := SE2->E2_CSLL

            SE2->E2_PRETPIS := " "
            SE2->E2_PRETCOF := " "
            SE2->E2_PRETCSL := " "

        EndIf

        MsUnlock()

    Endif

    nValPis     := SE2->E2_VRETPIS
    nValCofins  := SE2->E2_VRETCOF
    nValCsll    := SE2->E2_VRETCSL

    If SuperGetMv("MV_AG10925",.F.,"2") == "1" .and. cCodRetPis == "5952"
        nRefCof := nValCofins //Armazena o valor do Cofins, para recompor os valores apos a geracao dos titulos
        nRefCsl := nValCsll   //Armazena o valor do Csll, para recompor os valores apos a geracao dos titulos

        nValPis    += nValCofins + nValCsll
        nValCofins := 0
        nValCsll   := 0
    Endif

    // Verifica se houve alteracao de PIS
    SE2->(dbSetOrder(1))
    If (SE2->E2_PIS != nOldPisAnt .or. SE2->E2_CODRET<>cOldCodRet .or. nValorTit	> nVlMinImp) .and. !(SE2->E2_ORIGEM == "MATA100 " .and. !lPCCBaixa) .And. Iif(SuperGetMv("MV_AG10925",.F.,"2") == "1" .and. cCodRetPis == "5952", nValPis > 0,.T.)

        dbSelectArea("SE2")
        nRegSe2 := RecNo()
        //	nValPis:= SE2->E2_PIS
        If SuperGetMv("MV_AG10925",.F.,"2") <> "1"
            nValPis := SE2->E2_PIS
        EndIf
        If nOldPisAnt != 0
            If (dbSeek(xFilial("SE2")+cChavePis+cUniao))
                If nValPis != 0
                    If !__lPccMR
                        Reclock("SE2")
                        SE2->E2_VALOR := nValPis
                        SE2->E2_SALDO := nValPis
                        SE2->E2_VLCRUZ:= Round( nValPis, MsDecimais(1) )
                        // Trata a alteração do codigo de retenção *
                        SE2->E2_DIRF    := cGeraDirf
                        SE2->E2_CODRET  := cCodRetPis
                    EndIf
                    PCODetLan("000002","10","FINA050")		// Altera o lancamento de PIS gerado no PCO
                Else
                    PCODetLan("000002","10","FINA050",.T.)	// Apaga o lancamento de PIS gerado no PCO

                    If !__lPccMR
                        Iif(__lIntPFS .and. FindFunction("JDelTitCP"), JDelTitCP(SE2->(Recno())), Nil) // Integração SIGAPFS x SIGAFIN remove os desdobramentos quando o titulo for deletado
                        FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                        Reclock("SE2",.F.,.T.)
                        dbDelete()
                        msUnLock()

                        dbGoto (nRegSE2)
                        Reclock("SE2",.F.)
                        SE2->E2_PARCPIS := " "
                        SE2->E2_VRETPIS := 0
                        SE2->E2_PRETPIS := "1"
                        msUnLock()
                        lZerouImp := .T.
                    EndIf
                EndIf
            Else
                nOldPisAnt := 0
            EndIf
            dbGoto(nRegSe2)
        Endif
        // Verifica se informado PIS sem existir
        // anteriormente.
        If nOldPisAnt = 0 .And. SE2->E2_PIS != 0 .and. lRetParc .and. !lPccBaixa
            If !__lPccMR
                nValPis := SE2->E2_PIS
                //Gera titulo de PIS Cria o fornecedor, caso nao exista
                dbSelectArea("SA2")

                If !(dbSeek(xFilial("SA2")+GetMv("MV_UNIAO")))
                    Reclock("SA2",.T.)
                    Replace A2_FILIAL With xFilial("SA2")
                    Replace A2_COD    With GetmV("MV_UNIAO")
                    Replace A2_NOME	With "UNIAO"
                    Replace A2_NREDUZ With "UNIAO"
                    Replace A2_LOJA	With cLojaImp
                    Replace A2_MUN 	With "."
                    Replace A2_EST 	With SuperGetMv("MV_ESTADO")
                    Replace A2_BAIRRO With "."
                    Replace A2_END 	With "."
                    Replace A2_TIPO	With "J"
                EndIF

                dVencRea := F050VImp("PIS",dEmissao,dEmis1,dVctoReal) // Calcula o vencimento do imposto

                //Verifica parcela do PIS caso exista titulo de PIS com o mesmo numero.
                cParcPis := ParcImposto(cPrefixo,cNum,cTipoSE2)

                //Grava a parcela do PIS no titulo pai fazendo a amarracao titulo x titulo PIS
                dbGoto(nRegSe2)
                RecLock("SE2")
                SE2->E2_PARCPIS 	:= cParcPis
                SE2->E2_DIRF    	:= "2"	 // Desmarca titulo principal, pois apenas o titulo de
                // imposto var para DIRF
                //imposto var para DIRF Cria a natureza PIS caso nao exista
                dbSelectArea("SED")
                cVar := Alltrim(GetMv("MV_PISNAT"))
                cVar := cVar + Space(10-Len(cVar))
                If !(dbSeek(cFilial+cVar))
                    RecLock("SED",.T.)
                    Replace 	ED_FILIAL  With cFilial,;
                    ED_CODIGO  With cVar	,	;
                    ED_CALCIRF With "N" 	,	;
                    ED_CALCISS With "N"	, 	;
                    ED_CALCINS With "N"	,	;
                    ED_CALCCSL With "N"  ,	;
                    ED_CALCCOF With "N"  ,  ;
                    ED_CALCPIS With "N"  ,	;
                    ED_DESCRIC With "PIS",  ;
                    ED_TIPO	   With "2"
                    MsUnlock()

                    Iif(__lIntPFS .and. FindFunction("JurCompSED"), JurCompSED(SED->(Recno())), Nil) //Integração SIGAPFS - Complemento da Natureza
                EndIf
                // Grava titulo de PIS caso n„o exista anterior.
                RecLock("SE2",.T.)
                SE2->E2_FILIAL		:= cFilial
                SE2->E2_PREFIXO 	:= cPrefixo
                SE2->E2_NUM			:= cNum
                SE2->E2_PARCELA 	:= cParcPis
                SE2->E2_NATUREZ 	:= GetMv("MV_PISNAT")
                SE2->E2_TIPO		:= Iif(cTipoSE2 $ MVPAGANT+"/"+MV_CPNEG  .And. !lPCCBaixa,MVTXA,MVTAXA)
                SE2->E2_EMISSAO 	:= dEmissao
                SE2->E2_VALOR		:= nValPis
                SE2->E2_VENCREA 	:= dVencrea
                SE2->E2_SALDO		:= nValPis
                SE2->E2_VENCTO		:= dVencRea
                SE2->E2_VENCORI 	:= dVencRea
                SE2->E2_EMIS1		:= IIf(Type("dDataEmis1") # "U",IIf(!Empty(dDataEmis1),dDataEmis1,dDataBase),dDataBase)
                SE2->E2_FORNECE		:= GetMv("MV_UNIAO")
                SE2->E2_LOJA 		:= cLojaImp
                SE2->E2_NOMFOR		:= SA2->A2_NREDUZ
                SE2->E2_MOEDA		:= 1
                SE2->E2_VLCRUZ		:= Round( nValPis, MsDecimais(1) )
                //Grava campo E2_TITPAI
                SE2->E2_TITPAI      := cTitPai
                SE2->E2_CODAPRO  := cCodAprov

                If lSpbInUse
                    Replace	SE2->E2_MODSPB with cModSpb
                Endif

                SE2->E2_FILORIG  := If(Empty(SE2->E2_FILORIG),cFilAnt,SE2->E2_FILORIG)
                SE2->E2_DIRF    := cGeraDirf
                SE2->E2_CODRET  := cCodRetPis
                SE2->(MsUnlock())
            EndIf

            // Grava o lancamento de PIS no PCO
            PCODetLan("000002","10","FINA050")

        EndIf
        dbSelectArea("SE2")
        dbGoto(nRegSe2)
    EndIf
    // Verifica se houve alteracao de COFINS
    If (SE2->E2_COFINS != nOldCofAnt .or. SE2->E2_CODRET<>cOldCodRet .or. nValorTit	> nVlMinImp) .and. !(SE2->E2_ORIGEM == "MATA100 " .and. !lPCCBaixa) .and. Iif(SuperGetMv("MV_AG10925",.F.,"2") == "1" .and. cCodRetPis == "5952", nValCofins > 0,.T.)
        dbSelectArea("SE2")
        nRegSe2 := RecNo()
        //nValCofins:= SE2->E2_COFINS
        If SuperGetMv("MV_AG10925",.F.,"2") <> "1"
            nValCofins:= SE2->E2_COFINS
        EndIf
        If nOldCofAnt != 0
            If (dbSeek(xFilial("SE2")+cChaveCof+cUniao))
                If nValCofins != 0
                    If !__lPccMR
                        Reclock("SE2")
                        SE2->E2_VALOR := nValCofins
                        SE2->E2_SALDO := nValCofins
                        SE2->E2_VLCRUZ:= Round( nValCofins, MsDecimais(1) )

                        // Trata a alteração do codigo de retenção *
                        SE2->E2_DIRF    := cGeraDirf
                        SE2->E2_CODRET  := cCodRetCof
                    EndIf

                    PCODetLan("000002","11","FINA050")		// Altera o lancamento de COFINS gerado no PCO
                Else
                    PCODetLan("000002","11","FINA050",.T.)	// Apaga o lancamento de COFINS gerado no PCO

                    If !__lPccMR
                        Iif(__lIntPFS .and. FindFunction("JDelTitCP"), JDelTitCP(SE2->(Recno())), Nil) // Integração SIGAPFS x SIGAFIN remove os desdobramentos quando o titulo for deletado
                        FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                        Reclock("SE2",.F.,.T.)
                        dbDelete()
                        msUnLock()

                        dbGoto (nRegSE2)
                        Reclock("SE2",.F.)
                        SE2->E2_PARCCOF := " "
                        SE2->E2_VRETCOF := 0
                        SE2->E2_PRETCOF := "1"
                        msUnLock()
                        lZerouImp := .T.
                    EndIf
                EndIf
            Else
                nOldCofAnt := 0
            EndIf
            dbGoto(nRegSe2)
        Endif
        // Verifica se informado COFINS sem existir
        // anteriormente.
        If nOldCofAnt = 0 .And. SE2->E2_COFINS != 0 .and. lRetParc .and. !lPccBaixa
            nValCofins := SE2->E2_COFINS

            If !__lPccMR
                // Gera titulo de COFINS Cria o fornecedor, caso nao exista
                dbSelectArea("SA2")
                If !(dbSeek(xFilial("SA2")+GetMv("MV_UNIAO")))
                    Reclock("SA2",.T.)
                    Replace A2_FILIAL With xFilial("SA2")
                    Replace A2_COD    With GetmV("MV_UNIAO")
                    Replace A2_NOME	With "UNIAO"
                    Replace A2_NREDUZ With "UNIAO"
                    Replace A2_LOJA	With cLojaImp
                    Replace A2_MUN 	With "."
                    Replace A2_EST 	With SuperGetMv("MV_ESTADO")
                    Replace A2_BAIRRO With "."
                    Replace A2_END 	With "."
                    Replace A2_TIPO	With "J"
                EndIF

                dVencRea := F050VImp("COFINS",dEmissao,dEmis1,dVctoReal) // Calcula o vencimento do imposto

                // Verifica parcela do COFINS caso exista titulo de COFINS com o mesmo numero.
                cParcCof := ParcImposto(cPrefixo,cNum,cTipoSE2)

                //Grava a parcela do COFINS no titulo pai fazendo a amarracao titulo x titulo COFINS
                RecLock("SE2")
                SE2->E2_PARCCOF 	:= cParcCof
                SE2->E2_DIRF    	:= "2"	 // Desmarca titulo principal, pois apenas o titulo de

                // imposto var para DIRF
                // Cria a natureza COFINS caso nao exista
                dbSelectArea("SED")
                cVar := Alltrim(GetMv("MV_COFINS"))
                cVar := cVar + Space(10-Len(cVar))

                If !(dbSeek(cFilial+cVar))
                    RecLock("SED",.T.)
                    Replace 	ED_FILIAL  With cFilial,;
                    ED_CODIGO  With cVar	,	;
                    ED_CALCIRF With "N" 	,	;
                    ED_CALCISS With "N"	, 	;
                    ED_CALCINS With "N"	,	;
                    ED_CALCCSL With "N"  ,	;
                    ED_CALCCOF With "N"  ,  ;
                    ED_CALCPIS With "N"  ,	;
                    ED_DESCRIC With "COFINS" , ;
                    ED_TIPO	   With "2"
                    MsUnlock()

                    Iif(__lIntPFS .and. FindFunction("JurCompSED"), JurCompSED(SED->(Recno())), Nil) //Integração SIGAPFS - Complemento da Natureza
                EndIf
                // Grava titulo de COFINS caso n„o exista anterior.
                RecLock("SE2",.T.)
                SE2->E2_FILIAL		:= cFilial
                SE2->E2_PREFIXO 	:= cPrefixo
                SE2->E2_NUM			:= cNum
                SE2->E2_PARCELA 	:= cParcCof
                SE2->E2_NATUREZ 	:= GetMv("MV_COFINS")
                SE2->E2_TIPO	   := Iif(cTipoSE2 $ MVPAGANT+"/"+MV_CPNEG  .And. !lPCCBaixa,MVTXA,MVTAXA)
                SE2->E2_EMISSAO 	:= dEmissao
                SE2->E2_VALOR		:= nValCofins
                SE2->E2_VENCREA 	:= dVencrea
                SE2->E2_SALDO		:= nValCofins
                SE2->E2_VENCTO		:= dVencRea
                SE2->E2_VENCORI 	:= dVencRea
                SE2->E2_EMIS1		:= IIf(Type("dDataEmis1") # "U",IIf(!Empty(dDataEmis1),dDataEmis1,dDataBase),dDataBase)
                SE2->E2_FORNECE	:= GetMv("MV_UNIAO")
                SE2->E2_LOJA 		:= cLojaImp
                SE2->E2_NOMFOR		:= SA2->A2_NREDUZ
                SE2->E2_MOEDA		:= 1
                SE2->E2_VLCRUZ		:= Round( nValCofins, MsDecimais(1) )
                //Grava campo E2_TITPAI
                SE2->E2_TITPAI      := cTitPai
                SE2->E2_CODAPRO  := cCodAprov

                If lSpbInUse
                    Replace	SE2->E2_MODSPB with cModSpb
                Endif

                SE2->E2_FILORIG  := If(Empty(SE2->E2_FILORIG),cFilAnt,SE2->E2_FILORIG)
                SE2->E2_DIRF    := cGeraDirf
                SE2->E2_CODRET  := cCodRetCof
                SE2->(MsUnlock())
            EndIf

            // Gera o lancamento de COFINS no PCO
            PCODetLan("000002","11","FINA050")

        EndIf
        dbSelectArea("SE2")
        dbGoto(nRegSe2)
    EndIf
    // Verifica se houve alteracao de CSLL
    If (SE2->E2_CSLL != nOldCslAnt .or. SE2->E2_CODRET<>cOldCodRet .or. nValorTit > nVlMinImp) .and. !(SE2->E2_ORIGEM == "MATA100 " .and. !lPCCBaixa) .And. Iif(SuperGetMv("MV_AG10925",.F.,"2") == "1" .and. cCodRetPis == "5952", nValCsll > 0,.T.)
        dbSelectArea("SE2")
        nRegSe2 := RecNo()
        //	nValCsll:= SE2->E2_CSLL
        If SuperGetMv("MV_AG10925",.F.,"2") <> "1"
            nValCsll:= SE2->E2_CSLL
        EndIf
        If nOldCslAnt != 0
            If (dbSeek(xFilial("SE2")+cChaveCsl+cUniao))
                If nValCsll != 0
                    If !__lPccMR
                        Reclock("SE2")
                        SE2->E2_VALOR := nValCsll
                        SE2->E2_SALDO := nValCsll
                        SE2->E2_VLCRUZ:= Round( nValCsll, MsDecimais(1) )
                        // Trata a alteração do codigo de retenção *
                        SE2->E2_DIRF    := cGeraDirf
                        SE2->E2_CODRET  := cCodRetCsl
                    EndIf

                    PCODetLan("000002","12","FINA050")		// Altera o lancamento de CSLL gerado no PCO
                Else
                    PCODetLan("000002","12","FINA050",.T.)	// Apaga o lancamento de CSLL gerado no PCO

                    If !__lPccMR

                        Iif(__lIntPFS .and. FindFunction("JDelTitCP"), JDelTitCP(SE2->(Recno())), Nil) // Integração SIGAPFS x SIGAFIN remove os desdobramentos quando o titulo for deletado
                        FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                        Reclock("SE2",.F.,.T.)
                        dbDelete()
                        msUnLock()
                        dbGoto (nRegSE2)
                        Reclock("SE2",.F.)
                        SE2->E2_PARCSLL := " "
                        SE2->E2_VRETCSL := 0
                        SE2->E2_PRETCSL := "1"
                        msUnLock()
                        lZerouImp := .T.
                    EndIf
                EndIf
            Else
                nOldCslAnt := 0
            EndIf
            dbGoto(nRegSe2)
        Endif
        // Verifica se informado CSLL sem existir anteriormente.
        If nOldCslAnt = 0 .And. SE2->E2_CSLL != 0 .and. lRetParc .and. !lPccBaixa
            If !__lPccMR
                nValCsll := SE2->E2_CSLL
                //Gera titulo de CSLL Cria o fornecedor, caso nao exista
                dbSelectArea("SA2")

                If !(dbSeek(xFilial("SA2")+GetMv("MV_UNIAO")))
                    Reclock("SA2",.T.)
                    Replace A2_FILIAL With xFilial("SA2")
                    Replace A2_COD    With GetmV("MV_UNIAO")
                    Replace A2_NOME	With "UNIAO"
                    Replace A2_NREDUZ With "UNIAO"
                    Replace A2_LOJA	With cLojaImp
                    Replace A2_MUN 	With "."
                    Replace A2_EST 	With SuperGetMv("MV_ESTADO")
                    Replace A2_BAIRRO With "."
                    Replace A2_END 	With "."
                    Replace A2_TIPO	With "J"
                EndIF

                dVencRea := F050VImp("CSLL",dEmissao,dEmis1,dVctoReal) // Calcula o vencimento do imposto

                //Verifica parcela do CSLL caso exista titulo de CSLL com o mesmo numero.
                cParcCsll := ParcImposto(cPrefixo,cNum,cTipoSE2)

                //Grava a parcela do CSLL no titulo pai fazendo a amarracao titulo x titulo CSLL
                dbGoto(nRegSe2)
                RecLock("SE2")
                SE2->E2_PARCSLL 	:= cParcCsll
                SE2->E2_DIRF    	:= "2"	 // Desmarca titulo principal, pois apenas o titulo de

                //imposto vai para DIRF Cria a natureza CSLL caso nao exista
                dbSelectArea("SED")
                cVar := Alltrim(GetMv("MV_CSLL"))
                cVar := cVar + Space(10-Len(cVar))

                If !(dbSeek(cFilial+cVar))
                    RecLock("SED",.T.)
                    Replace 	ED_FILIAL  With cFilial,;
                    ED_CODIGO  With cVar	,	;
                    ED_CALCIRF With "N" 	,	;
                    ED_CALCISS With "N"	, 	;
                    ED_CALCINS With "N"	,	;
                    ED_CALCCSL With "N"  ,	;
                    ED_CALCCOF With "N"  ,  ;
                    ED_CALCPIS With "N"  ,	;
                    ED_DESCRIC With "CSLL", ;
                    ED_TIPO	   With "2"
                    MsUnlock()

                    Iif(__lIntPFS .and. FindFunction("JurCompSED"), JurCompSED(SED->(Recno())), Nil) //Integração SIGAPFS - Complemento da Natureza
                EndIf

                // Grava titulo de CSLL caso n„o exista anterior.
                RecLock("SE2",.T.)
                SE2->E2_FILIAL		:= cFilial
                SE2->E2_PREFIXO 	:= cPrefixo
                SE2->E2_NUM			:= cNum
                SE2->E2_PARCELA 	:= cParcCsll
                SE2->E2_NATUREZ 	:= GetMv("MV_CSLL")
                SE2->E2_TIPO		:= Iif(cTipoSE2 $ MVPAGANT+"/"+MV_CPNEG  .And. !lPCCBaixa,MVTXA,MVTAXA)
                SE2->E2_EMISSAO 	:= dEmissao
                SE2->E2_VALOR		:= nValCsll
                SE2->E2_VENCREA 	:= dVencrea
                SE2->E2_SALDO		:= nValCsll
                SE2->E2_VENCTO		:= dVencRea
                SE2->E2_VENCORI 	:= dVencRea
                SE2->E2_EMIS1		:= IIf(Type("dDataEmis1") # "U",IIf(!Empty(dDataEmis1),dDataEmis1,dDataBase),dDataBase)
                SE2->E2_FORNECE		:= GetMv("MV_UNIAO")
                SE2->E2_LOJA 		:= cLojaImp
                SE2->E2_NOMFOR		:= SA2->A2_NREDUZ
                SE2->E2_MOEDA		:= 1
                SE2->E2_VLCRUZ		:= Round( nValCsll, MsDecimais(1) )
                //Grava campo E2_TITPAI
                SE2->E2_TITPAI      := cTitPai
                SE2->E2_CODAPRO  	:= cCodAprov

                If lSpbInUse
                    Replace	SE2->E2_MODSPB with cModSpb
                Endif

                SE2->E2_FILORIG  := If(Empty(SE2->E2_FILORIG),cFilAnt,SE2->E2_FILORIG)
                SE2->E2_DIRF    := cGeraDirf
                SE2->E2_CODRET  := cCodRetCsl
                SE2->(MsUnlock())
            EndIf

            // Gera o lancamento de CSLL no PCO
            PCODetLan("000002","12","FINA050")

        EndIf
        dbSelectArea("SE2")
        dbGoto(nRegSe2)

        If !__lPccMR .and. lZerouImp .and. !lPccBaixa
            aRecSE2 := FImpExcTit("SE2",SE2->E2_PREFIXO,SE2->E2_NUM,SE2->E2_PARCELA,SE2->E2_TIPO,SE2->E2_FORNECE,SE2->E2_LOJA)
            For nX := 1 to Len(aRecSE2)
                SE2->(MSGoto(aRecSE2[nX]))
                FaAvalSE2(4)
            Next

            // Exclui os registros de relacionamentos do SFQ
            SE2->(dbGoto(nRegSE2))
            FImpExcSFQ("SE2",SE2->E2_PREFIXO,SE2->E2_NUM,SE2->E2_PARCELA,SE2->E2_TIPO,SE2->E2_FORNECE,SE2->E2_LOJA)
        Endif
    EndIf

    //Refaz os valores de PIS/COFINS/CSLL, quando aglutinados em um unico TX.
    If SuperGetMv("MV_AG10925",.F.,"2") == "1" .and. cCodRetPis == "5952"
        nValPis     -= nRefCof + nRefCsl
        nValCsll    := nRefCsl
        nValCofins  := nRefCof
    EndIf

    If !__lPccMR .And. (SE2->E2_ORIGEM == "MATA100 " .or. (!lAlterNat .And. !lZerouImp .And. SE2->E2_EMISSAO < __dLastPCC)) .and. !lPCCBaixa
        RECLOCK("SE2",.f.)
        SE2->E2_PIS 	:= nPisOri
        SE2->E2_COFINS 	:= nCofOri
        SE2->E2_CSLL 	:= nCslOri
        MsUnlock()
    EndIf

    If !__lPccMR .and. lRestValImp .and. !lPccBaixa
        // Restaura os valores originais de PIS / COFINS / CSLL
        RecLock("SE2", .F. )

        If M->E2_PIS == 0 .Or. (nRetOriPIS <> nVlRetPIS .And. nVlRetPIS <> SE2->E2_PIS)
            SE2->E2_PIS    := If (!Empty(nRetOriPIS),nRetOriPIS,SE2->E2_PIS)
        EndIf

        If M->E2_COFINS == 0 .Or. (nRetOriCOF <> nVlRetCOF .And. nVlRetCOF <> SE2->E2_COFINS)
            SE2->E2_COFINS := If (!Empty(nRetOriCOF),nRetOriCOF,SE2->E2_COFINS)
        EndIf

        If M->E2_CSLL == 0 .Or. (nRetOriCSL <> nVlRetCSL .And. nVlRetCSL <> SE2->E2_CSLL)
            SE2->E2_CSLL   := If (!Empty(nRetOriCSL),nRetOriCSL,SE2->E2_CSLL)
        EndIf

        SE2->E2_VLCRUZ := Round(NoRound(xMoeda(SE2->E2_VALOR,SE2->E2_MOEDA,1,SE2->E2_EMISSAO,MsDecimais(1)+1,SE2->E2_TXMOEDA),MsDecimais(1)+1),MsDecimais(1))
    EndIf

    // Efetua a atualizacao dos arquivos do SIGAPMS
    // integração com o PMS
    If IntePMS() .And. SE2->E2_PROJPMS == "1" .and. !lRatAutPrj//so validar se nao for chamado pelo array automatico de rateio
        Eval(bPMSDlgFI)
    EndIf

    lPrimeiro:= .T. //Wilson em 06/06/2011

    If IntePMS() .and. SE2->E2_PROJPMS == "1"
        PmsWriteFI(2,"SE2")	//Estorno
        PmsWriteFI(1,"SE2") //Inclusao
    Endif

    // Efetua a gravacao das justificativas
    // Adiciona botao para envio de instrucoes de cobranca
    F050GrvFI2()

    // Grava os lancamentos nas contas orcamentarias SIGAPCO
    If SE2->E2_TIPO $ MVPAGANT
        PcoDetLan("000002","02","FINA050")
    Else
        PcoDetLan("000002","01","FINA050")
    EndIf

    //Chamada de funcao para tratamento da Average
    If lIntegracao
        FI400VALFIN()
    EndIF

    // Valores acessórios
    If lF050Auto .and. ValType(__aVAAuto) = "A" .and. FAPodeTVA(SE2->E2_TIPO, /*cNatureza*/, .F., "P")
        If !Fa050VA(.T.)
            lResult := .F.
        Endif
    Endif

    //realiza a gravacao da alteracao do model
    If __lLocBRA
        Fa986grava("SE2","FINA050")
    EndIf

    //realiza a integracao online do titulo para o TAF
    //habilitar somente quando tiver a integra??o TAF
    //If FindFunction("TAFExstInt") .And. TAFExstInt()
    //FinExpTAF(SE2->(Recno()),1,,,,, )
    //EndIf

    //Alteracao no titulo retido ou retentor com PCC retido na emissao
    If 	!lPccBaixa
        //Alteração de Valores (Titulo e/ou PCC)
        If	((nVlrOri != SE2->E2_VALOR) .Or. (SE2->E2_PIS != nPisOri .Or. SE2->E2_COFINS != nCofOri .Or. SE2->E2_CSLL != nCslOri)) .And. (SE2->E2_VENCREA 	>= dDataIni .And. SE2->E2_VENCREA <= dDataFim)
            //Verifica a possibilidade de Alteração de um titulo que teve seus impostos(PCC) Retidos em outro Titulo(Retentor)
            If F050VerAlt()
                //Caso seja permitida a alteracao de um titulo retido
                If	lTitRetA
                    F050AltRtd()

                    //Caso seja permitida a alteracao de um titulo retentor ou aguardando retenção
                ElseIf  (SE2->E2_PRETPIS <>	"2" .Or. SE2->E2_PRETCOF <>	"2"	.Or. SE2->E2_PRETCSL <>  "2")

                    //Verifico se o titulo eh retentor
                    lTitReteu := (SE2->E2_PRETPIS == " " .Or. SE2->E2_PRETCOF == " " .Or. SE2->E2_PRETCSL == " ")

                    aAreaSed := SED->(GetArea())
                    aAreaSa2 := SA2->(GetArea())
                    //Se nao tiver essa checagem no caso de uma alteracao efetuando a retencao dos valores do PCC na mao
                    //de um titulo que tenha uma natureza sem impostos os campos E2_PIS,E2_COFINS e E2_CSLL sao gravados zerados.
                    SA2->(dbSeek(xFilial("SA2") + SE2->(E2_FORNECE + E2_LOJA)))
                    SED->(dbSeek(xFilial("SED") + SE2->E2_NATUREZ))
                    If (SED->ED_CALCPIS == "S" .AND. SA2->A2_RECPIS  == "2" .OR. ;
                    SED->ED_CALCCOF == "S" .AND. SA2->A2_RECCOFI == "2" .OR. ;
                    SED->ED_CALCCSL == "S" .AND. SA2->A2_RECCSLL == "2" ) .AND. ;
                    !lTitReteu .And. !lZerouImp .And. lAlterNat

                        F050GrvRtr()
                    Endif
                    RestArea(aAreaSed)
                    RestArea(aAreaSa2)
                EndIf
            Endif
        Endif
        //Alteração do Periodo,Data Vencto (Titulo e/ou PCC)
        If	(SE2->E2_VENCREA 	< dDataIni 		.Or. SE2->E2_VENCREA > dDataFim)
            //Verifica a possibilidade de Alteração de um titulo que teve seus impostos(PCC)
            //Retidos em outro Titulo(Retentor)
            If F050VerAlt()
                //Caso seja permitida a alteracao de um titulo retido
                If	lTitRetA
                    //Antes de chamar a função para a geracao dos titulos do PCC (F050AlRtd2 + F050TxPCC) verificar necessidade da geracao
                    SA2->(dbSeek(xFilial("SA2") + SE2->(E2_FORNECE + E2_LOJA)))
                    SED->(dbSeek(xFilial("SED") + SE2->E2_NATUREZ))
                    If SED->ED_CALCPIS == "S"  .AND. SA2->A2_RECPIS == "2" .OR. ;
	                    SED->ED_CALCCOF == "S" .AND. SA2->A2_RECCOFI == "2" .OR. ;
	                    SED->ED_CALCCSL == "S"  .and. SA2->A2_RECCSLL == "2"
                        If M->E2_EMISSAO < __dLastPCC .Or. lEmpPub
                            F050AlRtd2(!lRetOutMod)
                        EndIf
                    EndIf
                    //Caso seja permitida a alteracao de um titulo retentor ou aguardando retenção
                ElseIf  (SE2->E2_PRETPIS <>	"2" .Or. SE2->E2_PRETCOF <>	"2" .Or. SE2->E2_PRETCSL <>	"2")
                    //Antes de chamar a função para a geracao dos titulos do PCC (F050GrRtr2 + F050TxPCC) verificar necessidade da geracao
                    SA2->(dbSeek(xFilial("SA2") + SE2->(E2_FORNECE + E2_LOJA)))
                    SED->(dbSeek(xFilial("SED") + SE2->E2_NATUREZ))
                    If SED->ED_CALCPIS == "S"  .AND. SA2->A2_RECPIS == "2" .OR. ;
	                    SED->ED_CALCCOF == "S" .AND. SA2->A2_RECCOFI == "2" .OR. ;
	                    SED->ED_CALCCSL == "S"  .and. SA2->A2_RECCSLL == "2"
                        If M->E2_EMISSAO < __dLastPCC .Or. lEmpPub
                            F050AlRtd2(!lRetOutMod)
                        EndIf
                    EndIf

                EndIf
            EndIf
        EndIf
    EndIf

    dbGoto(nRegSe2)
    // Gera LP 511 somente quando NAO FOI EFETUADO DESDOBRAMENTO, caso contrario o LP ja foi gerado no desdobramento
    If SE2->E2_DESDOBR != "S" .And. lPadrao .And. Empty(SE2->E2_ARQRAT)
        // Contabiliza o rateio
        cSeq := Fa050GerLc( cPadrao,cLote, "FINA050", 3, , , NIL, cProcPCO, cItemPCO, cRecPag )
        If !Empty(cSeq)
            RecLock("SE2")
            Replace E2_ARQRAT		With cSeq
            SE2->(MsUnLock())
        EndIf
    Endif

    // Integração com o SigaPfs
    If __lIntPFS .and. !(lResult := F050AtuPFS(4, SE2->(Recno())))
        DisarmTransaction()
        Break
    Endif

    //Gravação da FK3/FK4 para os impostos da emissão
    FxGrvImpE("SE2", nRegSE2, aImps, aRecImpos,.F. )

    //Motor de retenções
    If __lTemMR .And. __lGrvMR
        FinSetAPrc("FK2")
        FinGrvImp("1", nRegSE2, aImpos, SE2->E2_ORIGEM, (mv_par10 = 2 .And. mv_par06 = 2), {}, {}, .T., .F., .F., SE2->E2_EMISSAO, "", "")
        FinSetAPrc("")
    EndIf

    //-------------------------------------------------------------------------------------
    // Integração Gesplan - Update somente para atualização do timestamp do registro pai
    If __lGesplan .And. ( SE2->E2_TIPO $ MVABATIM )		

        If !Empty(SE2->E2_TITPAI) .And. SE2->(MsSeek(xFilial("SE2") + SE2->E2_TITPAI))
            FUpdStamp('SE2',SE2->(Recno()))
        EndIF	
        SE2->(MsGoto(nRegSE2)) //reposiciono no AB-
    EndIF      

Return ( lResult ) // FA050AxAlt

//-------------------------------------------------------
/*/{Protheus.doc} Fa050VerImp
Procura se titulo de Imposto tem chque - na Exclusao

@author Pilar S. Albaladejo.
@since 17/12/99
@version P12
/*/
//-------------------------------------------------------
Function Fa050VerImp()

    LOCAL nRegSE2
    Local lRet := .f.
    Local cPrefixo
    Local cNum
    Local cSEST	:= GetMv("MV_SEST",,"")
    Local cParcSES
    Local cParcIRF
    Local cParcINS
    Local cParcISS
    Local aParcelas
    Local aNaturezas
    Local nX
    Local cTitPai	:= ""
    Local aValPCC	:= {}
    Local cTipo

    dbSelectArea("SE2")
    dbSetOrder(1)
    nRegSE2:= Recno()

    IF !(SE2->E2_TIPO $ MVISS+"/"+MVTAXA+"/"+MVTXA+"/"+MVINSS +"/" + "SES"+"/"+"INA")

        cPrefixo := SE2->E2_PREFIXO
        cNum		:= SE2->E2_NUM
        cTipo		:= SE2->E2_TIPO
        cTitPai		:= AllTrim( SE2->( E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA ) )
        cParcIRF	:= SE2->E2_PARCIR
        cParcISS	:= SE2->E2_PARCISS
        cParcINS	:= SE2->E2_PARCINS
        cParcSES 	:= SE2->E2_PARCSES

        If SE2->E2_ISS > 0
            If dbSeek(cFilial+cPrefixo+cNum+cParcISS+"ISS")
                While !Eof() .And. E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO == ;
                cFilial+cPrefixo+cNum+cParcISS+MVISS
                    IF AllTrim(E2_NATUREZ) = AllTrim(&(GetMv("MV_ISS")))
                        If SE2->E2_IMPCHEQ == "S" .And. AllTrim( SE2->E2_TITPAI ) == cTitPai
                            lRet := .T.
                        EndIf
                    EndIf
                    dbSkip()
                EndDo
            EndIf
        EndIf

        If SE2->E2_INSS > 0
            If !lRet
                If dbSeek(cFilial+cPrefixo+cNum+cParcINS+IF(cTipo$MVPAGANT,"INA","INS"))
                    While !Eof( ) .And. E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO == ;
                    cFilial+cPrefixo+cNum+cParcINS+IF(cTipo$MVPAGANT,"INA",MVINSS)
                        IF AllTrim(E2_NATUREZ) = AllTrim(&(GetMv("MV_INSS")))
                            If SE2->E2_IMPCHEQ == "S" .And. AllTrim( SE2->E2_TITPAI ) == cTitPai
                                lRet := .T.
                            EndIf
                        EndIf
                        dbSkip()
                    EndDo
                EndIf
            EndIf
        EndIf

        If SE2->E2_SEST > 0
            If !lRet
                If dbSeek(cFilial+cPrefixo+cNum+cParcSES+"SES")
                    While !Eof( ) .And. E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO == ;
                    cFilial+cPrefixo+cNum+cParcSES+"SES"
                        IF AllTrim(E2_NATUREZ) = AllTrim(cSEST)
                            If SE2->E2_IMPCHEQ == "S" .And. AllTrim( SE2->E2_TITPAI ) == cTitPai
                                lRet := .T.
                            EndIf
                        EndIf
                        dbSkip()
                    EndDo
                EndIf
            EndIf
        EndIf

        If SE2->E2_IRRF > 0
            If !lRet
                If dbSeek(cFilial+cPrefixo+cNum+cParcIRF+"TX ")
                    While !EOF() .And. E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO == ;
                    cFilial+cPrefixo+cNum+cParcIRF+MVTAXA
                        IF E2_NATUREZ = &(GetMv("MV_IRF"))
                            If SE2->E2_IMPCHEQ == "S" .And. AllTrim( SE2->E2_TITPAI ) == cTitPai
                                lRet := .T.
                            EndIf
                        EndIf
                        dbSkip()
                    EndDo
                EndIf
            EndIf
        EndIf

        If !lRet
            aValPCC   := { SE2->E2_PIS    , SE2->E2_COFINS , SE2->E2_CSLL }
            aParcelas := { SE2->E2_PARCPIS, SE2->E2_PARCCOF, SE2->E2_PARCSLL }
            aNaturezas := { GetMv("MV_PISNAT"), GetMv("MV_COFINS"), GetMv("MV_CSLL") }
            For nX := 1 To Len(aParcelas)
                If aValPCC[nX] > 0
                    If MsSeek(xFilial("SE2")+cPrefixo+cNum+aParcelas[nX]+MVTAXA)
                        While !EOF() .And. E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO == ;
                        cFilial+cPrefixo+cNum+aParcelas[nX]+MVTAXA
                            IF E2_NATUREZ = aNaturezas[nX]
                                If SE2->E2_IMPCHEQ == "S" .And. AllTrim( SE2->E2_TITPAI ) == cTitPai
                                    lRet := .T.
                                EndIf
                            EndIf
                            dbSkip()
                        EndDo
                    EndIf
                EndIf
            Next
        EndIf
    Endif
    dbSelectArea("SE2")
    dbGoto(nRegSE2)

Return lRet

//-------------------------------------------------------
/*/{Protheus.doc} fa050MCpo
Monta array com os campos que podera ser alterado
Criado para compatibilizacao com rotinas automaticas

@author Pilar S. Albaladejo.
@since 05/12/00
@version P12
/*/
//-------------------------------------------------------
Function fa050MCpo(nOpcAuto)

    Local aCpos := {}
    Local lSpbInUse := SpbInUse()
    Local lPode := .F.
    Local nX
    Local lAltLib := .T.
    //Controla o Pis Cofins e Csll na baixa
    Local lPCCBaixa := SuperGetMv("MV_BX10925",.T.,"2") == "1"
    Local lF050MCP := ExistBlock("F050MCP")
    Local lIs48 As Logical

    //Base IRPF na baixa
    Local lBaseIRPF	:= F050BIRPF(2)
    Local lBaseImp	:= F050BSIMP(2)	//Verifica a existência dos campos

    DEFAULT nOpcAuto := 3 // Rotinas automaticas sao por default Inclusao.

    //Permissao para altera
    If GETMV("MV_CTLIPAG")
        lAltLib := (SuperGetMv("MV_ALTLIPG",.F.,"S") == "S")
        //Se nao permite a alteracao verifico a liberacao.
        If !lAltLib
            //Se o titulo nao foi liberado, libero a alteracao
            If	Empty(SE2->E2_DATALIB)
                lAltLib := .T.
            Endif
        Endif
    Endif

    // Titulos com baixa ou titulo de ISS ou IR ou INSS
    // ou SEST podem ter alterados apenas alguns campos.

    If cPaisLoc == "RUS"
        lIs48 := R604Is48(SE2->E2_FILIAL,SE2->E2_PREFIXO,SE2->E2_NUM,SE2->E2_PARCELA,SE2->E2_TIPO,SE2->E2_FORNECE,SE2->E2_LOJA)
    Else
        lIs48 := .F.
    Endif

    If nOpcAuto != 3 .And.;
        (!Empty(SE2->E2_BAIXA) .or. SE2->E2_TIPO $ MVISS+"/"+MVTAXA+"/"+MVINSS+"/"+"SES"+"/"+MVTAXA+"/"+MVISS+"/"+MVTXA+"/"+"INA" .Or. ;
        "S" $ SE2->E2_LA .or. SE2->E2_IMPCHEQ == "S" .or. !lAltLib .or. "GPE" $ SE2->E2_ORIGEM .Or.;
        "S" $ SE2->E2_RATEIO .or. SE2->E2_FATURA = "NOTFAT" .or. F050BxImp() .or. ;
        ( SE2->E2_PRETPIS == "2" .or. SE2->E2_PRETCOF == "2" .or. SE2->E2_PRETCSL == "2") .or. (lIs48))

        If SE2->E2_SALDO = 0
            Help(" ",1,"FA050BAIXA")
            Return
        EndIf

        //Habilita os campos para Alteração
        //Não devem ser alterados titulos originados do modulo Compras que tiverem retencao de pcc na emissao.
        //Conforme chamado TRGZT8, passou-se a permitir a alteracao da data de vencimento, porem, os impostos nao sao recalculados.
        If F050VerAlt(.F.)
            AADD(aCpos,"E2_VENCTO")
            AADD(aCpos,"E2_VENCREA")
            lTitRetA 	:= .T.
        EndIF

        // Permite alterar os campos se o Titulo foi gerado:
        // no Modulo de Transporte, datas de Vencimento do Titulo.
        // no Modulo de TOTVSGFE, datas de Vencimento do Titulo, Codigo de Barras e Historico.
        If nOpcAuto == 4 .And. AllTrim(SE2->E2_ORIGEM) $ 'SIGATMS/TOTVSGFE'
            aCpos := {}
            If AllTrim(SE2->E2_ORIGEM) $ "SIGATMS"
                AADD(aCpos,"E2_VENCTO")
                AADD(aCpos,"E2_VENCREA")
            ElseIf AllTrim(SE2->E2_ORIGEM) $ "TOTVSGFE"
                AADD(aCpos,"E2_VENCTO")
                AADD(aCpos,"E2_VENCREA")
                AADD(aCpos,"E2_CODBAR")
                AADD(aCpos,"E2_PORTADO") //Portador
                AADD(aCpos,"E2_MODSPB") //Mod.Pagto
                AADD(aCpos,"E2_LINDIG") //Linha Dig
                AADD(aCpos,"E2_FORBCO") //Banco For
                AADD(aCpos,"E2_FORAGE") //Agencia For
                AADD(aCpos,"E2_FAGEDV") //DV Agencia
                AADD(aCpos,"E2_FORCTA") //Conta For
                AADD(aCpos,"E2_FCTADV") //DV Conta
                AADD(aCpos,"E2_FORMPAG") //Form Pag
            EndIf

            AADD(aCpos,"E2_HIST")
            Return aCpos
        EndIf

        If nOpcAuto == 4 .And. cModulo<>"EIC" .AND. AllTrim(SE2->E2_ORIGEM) $ 'SIGAEIC'
            aCpos := {}
            //TDF - 04/08/2011 - Quando o título é "INV" não pode alterar os campos "E2_VENCTO" e "E2_VENCREA"
            If SE2->E2_TIPO <> "INV"
                AADD(aCpos,"E2_VENCTO")
                AADD(aCpos,"E2_VENCREA")
            EndIf
            AADD(aCpos,"E2_HIST")
            AADD(aCpos,"E2_PORTADO")
            AADD(aCpos,"E2_FLUXO")
            AADD(aCpos,"E2_DIRF")
            AADD(aCpos,"E2_CODBAR")

            Return aCpos
        EndIf

        AADD(aCpos,"E2_HIST")
        AADD(aCpos,"E2_INDICE")
        AADD(aCpos,"E2_OP")
        AADD(aCpos,"E2_PORTADO")
        AADD(aCpos,"E2_FLUXO")
        AADD(aCpos,"E2_VALJUR")
        AADD(aCpos,"E2_PORCJUR")
        AADD(aCpos,"E2_CODRET")
        If !F050BxImp()
            AADD(aCpos,"E2_DIRF")
        EndIf
        AADD(aCpos,"E2_CODBAR")
        AADD(aCpos,"E2_LINDIG")

        If lSpbInUse
            AADD(aCpos,"E2_MODSPB")
        Endif

        // So permite alterar a natureza, depois de contabilizado o titulo, se ela nao estiver
        // preenchida
        If SED->(MsSeek(xFilial("SED")+SE2->E2_NATUREZ))
            For nX := 1 To SED->(FCount())
                If "_CALC" $ SED->(FieldName(nX))
                    lPode := !SED->(FieldGet(nX)) $ "1S" // So permite alterar se nao calcular impostos
                    If !lPode // No primeiro campo que calcula impostos, nao permite alterar
                        Exit
                    Else
                        // Não permite alterar em casos de titulos gerados por outros módulos que possuem impostos
                        If !("FINA" $ Upper(SE2->E2_ORIGEM)) .And. ;
                        ((SE2->E2_IRRF+SE2->E2_ISS+SE2->E2_INSS+SE2->E2_SEST+SE2->E2_PIS+SE2->E2_COFINS+SE2->E2_CSLL) > 0)
                            lPode := .F.
                            Exit
                        Endif
                    Endif
                Endif
            Next
        Endif
        If Empty(SE2->E2_NATUREZ) .Or. (lPode .and. Empty(SE2->E2_BAIXA))
            Aadd(aCpos,"E2_NATUREZ")
        Endif
        //So permite alterar os campos abaixo se não houve baixa, ainda que tenha sido contabilizada
        //a inclusao do mesmo
        If Empty(SE2->E2_BAIXA)
            AADD(aCpos,"E2_ACRESC")
            AADD(aCpos,"E2_DECRESC")
        Endif

        If cPaisLoc $ "ANG|ARG|AUS|BOL|BRA|CHI|COL|COS|DOM|EQU|EUA|HAI|MEX|PAD|PAN|PAR|PER|POR|PTG|SAL|TRI|URU|VEN"
            AADD(aCpos,"E2_VARIAC")
        EndIf
        If cPaisLoc $ "ANG|ARG|AUS|BOL|BRA|CHI|COL|COS|DOM|EQU|EUA|HAI|MEX|PAD|PAN|PAR|PER|POR|PTG|SAL|URU|VEN"
            AADD(aCpos,"E2_PERIOD")
        EndIf

        If lBaseIrpf .and. lPode
            AADD(aCpos,"E2_BASEIRF")
        Endif
        AADD(aCpos,"E2_FORMPAG")
    Else
        // Permite alterar os campos se o Titulo foi gerado:
        // no Modulo de Transporte, datas de Vencimento do Titulo.
        // no Modulo de TOTVSGFE, datas de Vencimento do Titulo, Codigo de Barras e Historico.
        If nOpcAuto == 4 .And. AllTrim(SE2->E2_ORIGEM) $ 'SIGATMS/TOTVSGFE'
            aCpos := {}
            If AllTrim(SE2->E2_ORIGEM) $ "SIGATMS"
                AADD(aCpos,"E2_VENCTO")
                AADD(aCpos,"E2_VENCREA")
            ElseIf AllTrim(SE2->E2_ORIGEM) $ "TOTVSGFE"
                AADD(aCpos,"E2_VENCTO")
                AADD(aCpos,"E2_VENCREA")
                AADD(aCpos,"E2_CODBAR")
                AADD(aCpos,"E2_PORTADO") //Portador
                AADD(aCpos,"E2_MODSPB") //Mod.Pagto
                AADD(aCpos,"E2_LINDIG") //Linha Dig
                AADD(aCpos,"E2_FORBCO") //Banco For
                AADD(aCpos,"E2_FORAGE") //Agencia For
                AADD(aCpos,"E2_FAGEDV") //DV Agencia
                AADD(aCpos,"E2_FORCTA") //Conta For
                AADD(aCpos,"E2_FCTADV") //DV Conta
                AADD(aCpos,"E2_FORMPAG") //Form Pag
            EndIf

            AADD(aCpos,"E2_HIST")
            Return aCpos
        EndIf

        If nOpcAuto == 4 .And. cModulo<>"EIC" .AND. AllTrim(SE2->E2_ORIGEM) $ 'SIGAEIC'
            aCpos := {}
            //TDF - 04/08/2011 - Quando o título é "INV" não pode alterar os campos "E2_VENCTO" e "E2_VENCREA"
            If SE2->E2_TIPO <> "INV"
                AADD(aCpos,"E2_VENCTO")
                AADD(aCpos,"E2_VENCREA")
            EndIf
            AADD(aCpos,"E2_HIST")
            AADD(aCpos,"E2_PORTADO")
            AADD(aCpos,"E2_FLUXO")
            AADD(aCpos,"E2_DIRF")
            AADD(aCpos,"E2_CODBAR")
            Return aCpos
        EndIf

        AADD(aCpos,"E2_VENCTO")
        AADD(aCpos,"E2_VENCREA")
        AADD(aCpos,"E2_HIST")
        AADD(aCpos,"E2_INDICE")
        AADD(aCpos,"E2_OP")
        AADD(aCpos,"E2_PORTADO")
        AADD(aCpos,"E2_VALJUR")
        AADD(aCpos,"E2_PORCJUR")

        // Não libera alteração do campo E2_VALOR caso de Integração Controle Orçamentário SIGAPFS x SIGAFIN e conforme os parametros MV_NRASDSD e MV_BLVLDES
        If AllTrim(SE2->E2_ORIGEM) <> 'JURCTORC' .and. VldRasDsd(SE2->E2_DESDOBR)
            AADD(aCpos,"E2_VALOR")
        EndIf

        AADD(aCpos,"E2_IRRF")
        AADD(aCpos,"E2_ISS")
        AADD(aCpos,"E2_FLUXO")
        AADD(aCpos,"E2_INSS")
        AADD(aCpos,"E2_ACRESC")
        AADD(aCpos,"E2_DECRESC")
        AADD(aCpos,"E2_CODRET")
        AADD(aCpos,"E2_DIRF")
        AADD(aCpos,"E2_LINDIG")
        AADD(aCpos,"E2_FORMPAG")
        AADD(aCpos,"E2_RATEIO")

        If SE2->E2_LA != "S"
            AADD( aCpos , "E2_CONTAD")
            AADD( aCpos , "E2_DEBITO")
            AADD( aCpos , "E2_CCUSTO")
            AADD( aCpos , "E2_CCD")
            AADD( aCpos , "E2_CCC")
            AADD( aCpos , "E2_ITEMD")
            AADD( aCpos , "E2_ITEMC")
            AADD( aCpos , "E2_CLVLDB")
            AADD( aCpos , "E2_CLVLCR")
        EndIf

        If lSpbInUse
            AADD(aCpos,"E2_MODSPB")
        Endif

        AADD(aCpos,"E2_CODBAR")

        If !lPccBaixa
            AAdd(aCpos, "E2_PIS")
            AAdd(aCpos, "E2_COFINS")
            AAdd(aCpos, "E2_CSLL")
        Endif

        // Nao permite alterar a natureza do titulo que reteve os impostos PIS/COFINS/CSL
        // do periodo, dele e de outros titulos.
        If SED->(MsSeek(xFilial("SED")+SE2->E2_NATUREZ))
            If !((SED->ED_CALCPIS == "S" .OR. SED->ED_CALCCSL == "S" .OR. SED->ED_CALCCOF == "S") .and. ;
            (SE2->(E2_VRETPIS+E2_VRETCOF+E2_VRETCSL) > 0 .and. ;
            STR(SE2->(E2_VRETPIS+E2_VRETCOF+E2_VRETCSL),17,2) != STR(SE2->(E2_PIS+E2_COFINS+E2_CSLL),17,2)))
                Aadd(aCpos,"E2_NATUREZ")
            Endif
        ElseIf Empty(SE2->E2_NATUREZ)
            Aadd(aCpos,"E2_NATUREZ")
        Endif

        If lBaseIrpf
            AADD(aCpos,"E2_BASEIRF")
        Endif

        If lBaseImp
            AADD(aCpos,"E2_BASEIRF")
            AADD(aCpos,"E2_BASEPIS")
            AADD(aCpos,"E2_BASEINS")
            AADD(aCpos,"E2_BASEISS")
        Endif

    EndIf
    If __lLocBRA
        Aadd(aCpos,"E2_NUMPRO")
        Aadd(aCpos,"E2_INDPRO")
        Aadd(aCpos,"E2_FORBCO")
        Aadd(aCpos,"E2_FORAGE")
        Aadd(aCpos,"E2_FAGEDV")
        Aadd(aCpos,"E2_FORCTA")
        Aadd(aCpos,"E2_FCTADV")
        AADD(aCpos,"E2_DTAPUR")
        AADD(aCpos,"E2_RETINS")
    Endif

    If  SE2->( FieldPos( "E2_MSBLQD" ) )>0
        AADD(aCpos,"E2_MSBLQD")
    EndIf

    If  SE2->( FieldPos( "E2_MSBLQL" ) )>0
        AADD(aCpos,"E2_MSBLQL")
    EndIf

    If lF050MCP
        aCpos := ExecBlock("F050MCP",.F.,.F.,aCpos)
    Endif
    If cPaisLoc == "RUS"
        Aadd(aCpos,"E2_FORBCO")
        Aadd(aCpos,"E2_FORAGE")
        Aadd(aCpos,"E2_FORCTA")
    Endif

    If __lRatMNat .And. SE2->E2_MULTNAT == "2"
        AADD(aCpos, "E2_MULTNAT")
    EndIf

    If (SE2->E2_LA != "S") .And. Empty(SE2->E2_ARQRAT)
        AADD(aCpos,"E2_RATEIO")
    Endif

Return aCpos

//-------------------------------------------------------
/*/{Protheus.doc} F050EscRat
Escolhe se digita rateio ou escolhe pre-configurado

@author Pilar S. Albaladejo.
@since 16/05/01
@version P12
/*/
//-------------------------------------------------------
Function F050EscRat(cPadrao As Character ,cProg As Character,cLote As Character)

    Local oDlg1         As Object
    Local oRadio        As Object
    Local cCodRateio	As Character
    Local nRadio		As Numeric
    Local nOpca 		As Numeric
    Local cHistorico 	As Character
    Local cSeq			As Character
    Local nIncAlt		As Numeric
    Local aRet			As Array    //variavel utilizada para o retorno do ponto de entrada F050RAUT
    Local nOpRat		As Numeric
    Local aRet2			As Array    //variavel utilizada para o retorno do ponto de entrada F050TMP1
    Local lF050RAUT 	As Logical
    Local nTamRat		As Numeric
    Local nTamCnt		As Numeric
    Local nTamHist      As Numeric

    Private cDebito	 	As Character
    Private cCredito 	As Character

    Default __lF50TMP1 := ExistBlock("F050TMP1")

    cCodRateio	:= CriaVar("CTJ_RATEIO")
    nRadio		:= 0
    nOpca 		:= 0
    cHistorico 	:= CriaVar("CT2_HIST")
    cSeq		:= ""
    nIncAlt		:= 3
    aRet		:= {} //variavel utilizada para o retorno do ponto de entrada F050RAUT
    nOpRat		:= 1
    aRet2		:= {}
    lF050RAUT 	:= ExistBlock("F050RAUT")
    nTamRat		:= TAMSX3("CTJ_RATEIO")[1]
    nTamCnt		:= TAMSX3("CT1_CONTA")[1]
    nTamHist    := TAMSX3("CTJ_HIST")[1]

    cDebito	 	:= CriaVar("CT2_DEBITO")
    cCredito 	:= CriaVar("CT2_CREDIT")

    lF050Auto := IF(Type("lF050Auto") == "U", .F., lF050Auto)

    If __lF50TMP1
        aRet2 := ExecBlock("F050TMP1",.f.,.f.,{/*nTipo*/,cCodRateio,cProg,cPadrao,cDebito,cCredito,cHistorico,lF050Auto,nOpRat})
    EndIf

    IF ( (empty(aRet2).Or.(aRet2[2] >= 1)) .And. !lF050Auto )  //Indica deseja abrir tela de Opcoes de rateio para rateio customizado.

        nRadio	:= 1

        DEFINE MSDIALOG oDlg1 FROM  94,1 TO 350,310 TITLE STR0088 PIXEL // "Opcoes de Rateio"

        @ 10,17 Say STR0089 SIZE 150,7 OF oDlg1 PIXEL  // "Escolha como Ratear"

        @ 27,07 TO 82, 150 OF oDlg1  PIXEL

        @ 35,10 Radio 	oRadio 	VAR nRadio;
            ITEMS 	STR0090,;		// "Digitado"
            STR0091 ;			// "Pre-Configurado"
            3D SIZE 100,10 OF oDlg1 PIXEL;
            ON CHANGE 	If(nRadio = 2, (oRateio:SetFocus(), .T.),;
            (cCodRateio := Space(Len(cCodRateio)), .T.)) .And.;
            (CtbDigCta(cCodRateio, oSayDeb, oDebito, oSayCrd, oCredito), .T.)
        @ 60,10 Say STR0092 PIXEL
        @ 60,50 MSGET oRateio Var cCodRateio F3 "CTJ" Picture "@!";
        SIZE 070,10 OF oDLG1 PIXEL When nRadio = 2;
        Valid CtbDigCta(cCodRateio, oSayDeb, oDebito, oSayCrd, oCredito,, .T.) HASBUTTON
        @ 87,07 	Say oSayDeb Prompt STR0117 OF oDlg1 PIXEL //"Conta a Debito"
        oSayDeb:Disable()
        @ 85,50 	MSGET oDebito Var cDebito;
        F3 "CT1" Picture "@!" Valid Ctb105Cta(cDebito) SIZE 070,8 OF oDlg1 PIXEL HASBUTTON
        oDebito:Disable()

        @ 102,07	Say oSayCrd Prompt STR0118 OF oDlg1 PIXEL //"Conta a Credito"
        oSayCrd:Disable()
        @ 100,50 	MSGET oCredito Var cCredito;
        F3 "CT1" Picture "@!" Valid Ctb105Cta(cCredito) SIZE 070,8 OF oDlg1 PIXEL HASBUTTON
        oCredito:Disable()
        @ 115,07	Say STR0119 OF oDlg1 PIXEL //"Historico"
        @ 115,50  	MSGET oHistorico Var cHistorico;
        Picture PesqPict("CT2", "CT2_HIST") SIZE 100,8 OF oDlg1 PIXEL

        DEFINE SBUTTON oBtn FROM 098,120 TYPE 1 ENABLE OF oDlg1;
        ACTION  Fa050ValRat(nRadio, cCodRateio, oDlg1, cDebito, cCredito, @nOpca)

        ACTIVATE MSDIALOG oDlg1 CENTERED

    Else
        nOpca := 1
    EndIf

    If nOpca == 1
        //O ponto de entrada F050RAUT recebe um array de tamanho 5 para alterar o conteudo das variaveis:
        // nRadio (Tipo de rateio), 
        // cCodRateio(Codigo do rateio), 
        // cHistorico (Historico do rateio),
        // cDebito (Conta Contábil Débito),
        // cCredito (Conta Contábil Credito)
        // Atribui verdadeiro na variavel lRatAut para nao mostrar a tela de rateio.
        If lF050RAUT .and. lF050Auto
            aRet := ExecBlock("F050RAUT",.f.,.f.)
            If ValType(aRet) = "A" .And. Len(aRet) >= 5
                nRadio		:= aRet[1]
                cCodRateio	:= PadR(aRet[2],nTamRat)
                cHistorico	:= PadR(aRet[3],nTamHist)
                cDebito		:= PadR(aRet[4],nTamCnt)
                cCredito	:= PadR(aRet[5],nTamCnt)
                lRatAut		:= .T.
            EndIf
        ElseIf lF050Auto .and. type("aItensCTB")=="A" .AND. Len(aItensCTB) > 0
            nRadio      := 1
            lRatAut		:= .T.
        EndIf
        cDebito  := Iif(nRadio = 1, "", cDebito)
        cCredito := Iif(nRadio = 1, "", cCredito)
        If cProg $ "FINA050/FINA100"
            If SuperGetMv("MV_FIRATD",.T.,"1") == "1"
                nIncAlt	:= If(nRadio=2,2,3)
            EndIf
        Endif
        cSeq		:= CtbRatFin(cPadrao,cProg,cLote,nRadio,cCodRateio,nIncAlt,cDebito,cCredito,cHistorico)
    ElseIf M->E2_RATEIO == "S"
        M->E2_RATEIO := "N"
    EndIf

Return cSeq

//-------------------------------------------------------
/*/{Protheus.doc} CtbRatFin
Rateio de Contas a Pagar

@author Pilar S. Albaladejo.
@since 11/05/01
@version P12
/*/
//-------------------------------------------------------
Function CtbRatFin( cPadrao As Character,;
                    cProg As Character,;
                    cLote As Character,;
                    nTipo As Numeric,;
                    cCodRateio As Character,;
                    nOpc As Numeric,;
                    cDebito As Character,;
                    cCredito As Character,;
                    cHistorico As Character,;
                    nHdlPrv As Numeric,;
                    nTotal As Numeric,;
                    aFlagCTB As Array,;
                    cProcPCO As Character,;
                    cItemPCO As Character,;
                    cRecPag As Character ) As Character

    Local lPanelFin     As Logical
    Local aCampos       As Array
    Local aSaveArea     As Array
    Local aRotAnt       As Array
    Local aTamQtd       As Array
    Local aAltera       As Array
    Local cSeq          As Character
    Local nTamQtd       As Numeric
    Local oDlg          As Object
    Local oGetDb        As Object
    Local lRatAut       As Logical
    Local nInss         As Numeric
    //Controla o Pis Cofins e Csll na baixa
    Local lPCCBaixa     As Logical
    Local lIRPFBaixa    As Logical
    Local lCalcIssBx    As Logical
    Local aSizeTela     As Array
    Local oPanel        As Object
    Local oTimer        As Object
    Local oPanel2       As Object
    Local nTimeOut      As Numeric
    Local nTimeMsg      As Numeric
    // informando que a tela fechará automaticamente em XX minutos
    Local aRecCV4       As Array
    Local nOpRat        As Numeric
    Local aRet          As Array
    Local lVisRateio    As Logical
    Local nTelaRat      As Numeric
    Local lTempBloq     As Logical
    Local nValorTela    As Numeric
    Local lF050RAUT     As Logical
    Local lUsaRatMem    As Logical

    Private aTela		As Array
    Private aGets		As Array
    Private aHeader		As Array

    Default nOpc		:= 3
    Default nHdlPrv 	:= 0
    Default nTotal    	:= 0
    Default __lF50TMP1  := ExistBlock("F050TMP1")

    Default aFlagCTB 	:= {}
    Default cProcPCO 	:= "000021"
    Default cItemPCO 	:= "01"
    Default cRecPag  	:= "P"

    lPanelFin   := IsPanelFin()
    aCampos     := {}
    aSaveArea   := GetArea()
    aRotAnt     := NIL   // Armazena conteudo da aRotina
    aTamQtd     := TAMSX3("CTJ_QTDTOT")
    aAltera     := {}
    cSeq        := ""
    nTamQtd     := aTamQtd[1]
    oDlg        := NIL
    oGetDb      := NIL
    lRatAut     := .F.
    nInss       := 0
    //Controla o Pis Cofins e Csll na baixa
    lPCCBaixa   := SuperGetMv("MV_BX10925",.T.,"2") == "1"
    lIRPFBaixa  := IIf( __lLocBRA, SA2->A2_CALCIRF == "2", .F.)
    lCalcIssBx  := IsIssBx("P")
    aSizeTela   := {}
    oPanel      := NIL
    oTimer      := NIL
    oPanel2     := NIL
    nTimeOut    := SuperGetMv("MV_FATOUT",,900)*1000 	// Estabelece 15 minutos para que o usuarios selecione os titulos a faturar
    nTimeMsg    := SuperGetMv("MV_MSGTIME",,120)*1000 	// Estabelece 02 minutos para exibir a mensagem para o usuário
    // informando que a tela fechará automaticamente em XX minutos
    aRecCV4     := {}
    nOpRat      := 2
    aRet        := {}
    lVisRateio  := ( nTipo == 5 .And. AllTrim( Upper( cProg ) ) == "FINA050" )
    nTelaRat    := 1
    lTempBloq   := .F.
    nValorTela  := 0
    lF050RAUT   := ExistBlock("F050RAUT")
    lUsaRatMem  := .F.
    aTela       := {}
    aGets       := {}
    aHeader     := {}

    // Obs: este array aRotina foi inserido apenas para permitir o
    // funcionamento das rotinas internas da MSGETDB
    If Type("aRotina") != "A"
        Private aRotina := { { "aRotina Falso", "AxInclui", 0 , nOpc} }
    Else
        aRotAnt		:= aClone(aRotina)
    Endif

    Private cPrograma	:= cProg
    Private nValRat	:= 0
    Private oValRat
    Private nTPRateio := nTipo // Tipo de rateio para que seja feito seu tratamento na validação das entidades contabéis adicionais

    Ctb120IniVar()

    lF100Auto := If(Type('lF100Auto') == "U", .F.,lF100Auto)
    lF050Auto := IF(Type("lF050Auto") == "U", .F., lF050Auto)

    lRatAut := Iif(ProcName(1)=="FA370PROCESSA",.F.,Iif(lF050Auto .OR. lF100Auto,.T.,.F.))

    // Cria aHeader
    aCampos := F050HeadCT(cPadrao,cProg,@aAltera,nTipo)

    // Caso o arquivo exista, o sistema apaga e reconstroi vazio.
    If Select("TMP") > 0 .and. nOpc#5
        If (( type("aItensCTB")=="A" .and. Len(aItensCTB) > 0 ) .or. (lF050Auto .and. lF050RAUT)) .Or. ( cProg $ "FINA370" .and. __lF50TMP1 .and. Len(aCampos) > 0 ) // Criar a TMP CTJ para não ocorrer error.log na contabilização Fina370 com o PE.
            F050Cria(aCampos)
        EndIf
        If !__lF50TMP1 .and. !lF050Auto
            If nOpc != 3 .Or. (nOpc == 3 .And. !Empty(TMP->(RecCount())) .And. !(lUsaRatMem := MsgYesNo(STR0132,STR0115))) // "Existe um rateio na memória. Deseja utilizá-lo?"#"Atenção"//Apaga TMP1            
                F050Cria(aCampos)
            Endif
        EndIf
    Else
        If nOpc#5 //Quando for exclusão a temporaria já estará criada
            F050Cria(aCampos) //Cria TMP1
        ElseIf nOpc == 5 .AND. AllTrim( Upper( cProg ) ) == "FINA100"
            F050Cria(aCampos) //Cria TMP1
        Endif
    EndIf

    //Indica se rateio Customizado, passando o lF050AUTO para identificação de rotina automática
    IF  __lF50TMP1 .And. TMP->(RecCount()) <= 0 .And. !lVisRateio
        aRet := ExecBlock("F050TMP1",.f.,.f.,{nTipo,cCodRateio,cProg,cPadrao,cDebito,cCredito,cHistorico,lF050Auto,nOpRat})
        If (ValType(aRet)=="A")
            nValRat  := aRet[1]
            nTelaRat := aRet[2]
        EndIf
        If nValRat == 0
            nValRat := F050Carr(nTipo,cCodRateio,cProg,cPadrao,cDebito,cCredito,cHistorico,aRecCV4,aCampos)
        EndIf
        IIF(lF050Auto ,lRatAut := .T., )
    ElseIf !lUsaRatMem
        nValRat := F050Carr(nTipo,cCodRateio,cProg,cPadrao,cDebito,cCredito,cHistorico,aRecCV4,aCampos)
        IIF(lF050Auto,lRatAut := .T., )
    Endif

    // Mostra o corpo da rateio
    nOpca := 0
    If cProg $ "FINA750/FINA050"
        If TMP->(Eof()) .And. TMP->CTJ_PERCEN ==0 .And. TMP->CTJ_VALOR == 0 .And. cProg $ "FINA050" //evitar error log caso seja inf primeiro a cta de deb ou cred
            Reclock("TMP", .T.)
            lTempBloq := .T.
        Endif
        nInss := M->E2_INSS
        IF SED->ED_DEDINSS == "2"  //desconta o INSS do principal
            nInss := 0
        Endif

        If	!lRatAut .And. ;
        ((nOpc !=5) .Or. (mv_par08 == 1 .And. nOpc == 5)) .And. ; //Se mostra tela de rateio na exclusao
        nTelaRat > 0

            aSizeTela := MSADVSIZE()

            DEFINE MSDIALOG oDlg TITLE STR0037 From aSizeTela[7],0 To aSizeTela[6],aSizeTela[5] OF oMainWnd PIXEL//"Rateios"
            oTimer:= TTimer():New((nTimeOut-nTimeMsg),{|| MsgTimer(nTimeMsg,oDlg) },oDlg) // Ativa timer
            oTimer:Activate()
            oDlg:lMaximized := .T.

            //TOPO DA TELA
            //---
            oPanel := TPanel():New(0,0,'',oDlg,, .T., .T.,, ,20,20,.T.,.T. )
            oPanel:Align := CONTROL_ALIGN_TOP

            @ 002 , 002		Say STR0021 + M->E2_FORNECE		FONT oDlg:oFont	OF oPanel PIXEL	// "Fornecedor: "
            @ 002 , 080		Say STR0022 + M->E2_LOJA			FONT oDlg:oFont	OF oPanel PIXEL	// "Loja: "
            @ 002 , 128  	Say STR0038 + M->E2_PREFIXO		FONT oDlg:oFont	OF oPanel PIXEL	// "Prefixo: "
            @ 002 , 175   	Say STR0039 + M->E2_NUM			FONT oDlg:oFont	OF oPanel PIXEL	// "N£mero T¡tulo: "
            @ 002 , 273   	Say STR0040 + M->E2_PARCELA		FONT oDlg:oFont	OF oPanel PIXEL	// "Parcela: "


            //GETDB - DIGITACAO
            //---
            oGetDB := 	MSGetDB():New(034,002,400,315,nOpc,"Fa050LinCT",{|| Fa050TudCT(nOpc,cPadrao,cProg,nTipo) },;
            "",.T.,aAltera,,.f.,,"TMP",,,.F.,,,,"FIN050DEL")

            n := TMP->(Reccount())
            oGetDB:ForceRefresh()
            oGetDB:lNewLine := .F.
            oGetDB:oBrowse:Align := CONTROL_ALIGN_ALLCLIENT
            //---

            //Se for Rateio digitado nao ira mostrar a quantidade total.
            oPanel2 := TPanel():New(0,0,'',oDlg,, .T., .T.,, ,20,20,.T.,.T. )

            @  001, 005 To  018,495 OF oPanel2 PIXEL

            nValorTela := Iif(mv_par06==1,;
            If(M->E2_MOEDA > 1 ,M->E2_VLCRUZ,M->E2_VALOR)+If(lIRPFBaixa,0,M->E2_IRRF)+If(!lCalcIssBx,M->E2_ISS,0)+ M->E2_INSS +M->E2_RETENC+M->E2_SEST+IIF(lPccBaixa,0,M->E2_PIS+M->E2_COFINS+M->E2_CSLL),;
            M->E2_VALOR)
            If mv_par06 == 1 .and. M->E2_MOEDA > 1
                nValorTela := Round(NoRound(xMoeda(nValorTela,1,M->E2_MOEDA,M->E2_EMISSAO,MsDecimais(1)+1,,M->E2_TXMOEDA),MsDecimais(1)+1),MsDecimais(1))
            EndIf

            If nTipo == 1
                @ 005 , 010	Say STR0041	FONT oDlg:oFont OF oPanel2 PIXEL  		// "Valor T¡tulo: "
                @ 005 , 042	Say nValorTela Picture PesqPict("SE2","E2_VALOR",17) 					FONT oDlg:oFont ;
                COLOR CLR_HBLUE OF oPanel2 PIXEL
            Else
                @ 005 , 010	Say STR0122	FONT oDlg:oFont OF oPanel2 PIXEL  		// "Quant. Total "
                @ 005 , 042	Say nQtdTot Picture PesqPict("CTJ","CTJ_QTDTOT",nTamQtd) FONT oDlg:oFont COLOR CLR_HBLUE OF oPanel2 PIXEL
                @ 005 , 130	Say STR0041	FONT oDlg:oFont OF oPanel2 PIXEL 		// "Valor T¡tulo: "
                @ 005 , 162	Say nValorTela Picture PesqPict("SE2","E2_VALOR",17) FONT oDlg:oFont COLOR CLR_HBLUE OF oPanel2 PIXEL
            EndIf
            If nOpc <> 5	//Se for Exclusao de titulo nao exibir o valor rateado
                @ 005 , 238	Say STR0042 FONT oDlg:oFont OF oPanel2 PIXEL 		// "Valor Rateio: "
                @ 005 , 270	Say oValRat VAR nValRat Picture PesqPict("CTJ","CTJ_VALOR",17)	FONT oDlg:oFont COLOR CLR_HBLUE OF oPanel2 PIXEL
            EndIf

            If lPanelFin  //Chamado pelo Painel Financeiro
                ACTIVATE MSDIALOG oDlg ON INIT (FaMyBar(oDlg,;
                {||nOpca:=1,if(nOpc = 2 .Or. fa050TudCT(nOpc,cPadrao,cProg,nTipo),oDlg:End(),nOpca := 0)},;
                {||nOpca:=2,fa050DelRat(),oDlg:End()}), oPanel2:Align := CONTROL_ALIGN_BOTTOM)
            Else
                ACTIVATE MSDIALOG oDlg ON INIT (EnchoiceBar(oDlg,;
                {||nOpca:=1,if(nOpc = 2 .Or. fa050TudCT(nOpc,cPadrao,cProg,nTipo,oGetDB),oDlg:End(),nOpca := 0)},;
                {||nOpca:=2,fa050DelRat(),oDlg:End()}), oPanel2:Align := CONTROL_ALIGN_BOTTOM)
            Endif
        Else
            nOpca :=1  
        Endif

        If lTempBloq
            TMP->(MsUnlock())
            lTempBloq := .F.
        EndIf

        If nOpca == 2 .And. M->E2_RATEIO == "S"
            M->E2_RATEIO := "N"
        EndIf
    ElseIf cProg == "FINA100"
        If !lRatAut
            aSizeTela := MSADVSIZE()

            DEFINE MSDIALOG oDlg TITLE STR0037 From aSizeTela[7],0 To aSizeTela[6],aSizeTela[5] OF oMainWnd PIXEL//"Rateios"
            oTimer:= TTimer():New((nTimeOut-nTimeMsg),{|| MsgTimer(nTimeMsg,oDlg) },oDlg) // Ativa timer
            oTimer:Activate()
            oDlg:lMaximized := .T.

            //TOPO DA TELA
            //---
            oPanel := TPanel():New(0,0,'',oDlg,, .T., .T.,, ,25,25,.T.,.T. )
            oPanel:Align := CONTROL_ALIGN_TOP

            @ 001,005 To  018,495 OF oPanel PIXEL
            @ 008,010 	SAY STR0043 + DtoC(M->E5_DATA)  FONT oDlg:oFont OF oPanel PIXEL	// "Data: "
            @ 008,190	SAY STR0044 + M->E5_DOCUMEN FONT oDlg:oFont OF oPanel PIXEL  		//"Doc.: "

            If nTipo == 1
                oGetDB := 	MSGetDB():New(034,005,400,315,nOpc,"Fa050LinCT",{|| Fa050TudCT(nOpc,cPadrao,cProg,nTipo) },;
                "",.T.,aAltera,,.f.,,"TMP",,,EMPTY(TMP->(RECCOUNT())),,,,"FIN050DEL")
            Else
                oGetDB := 	MSGetDB():New(034,005,400,315,nOpc,"Fa050LinCT",{|| Fa050TudCT(nOpc,cPadrao,cProg,nTipo) },;
                "",.T.,aAltera,,.f.,,"TMP",,,.F.,,,,"FIN050DEL")
            EndIf

            n := TMP->(Reccount())
            oGetDB:ForceRefresh()
            oGetDB:lNewLine := .F.
            oGetDB:oBrowse:Align := CONTROL_ALIGN_ALLCLIENT

            //---
            oPanel2 := TPanel():New(0,0,'',oDlg, oDlg:oFont, .T., .T.,, ,30,30,.T.,.T. )
            IF !lPanelFin
                oPanel2:Align := CONTROL_ALIGN_BOTTOM
            Endif


            //RODAPE
            //---
            //Se for Rateio digitado nao ira mostrar a quantidade total.
            @ 001, 005 To  018,495 OF oPanel2 PIXEL
            If nTipo == 1
                @ 004 , 010 	Say STR0041	FONT oDlg:oFont OF oPanel2 PIXEL  		// "Valor T¡tulo: "
                @ 004 , 042 	Say M->E5_VALOR			Picture PesqPict("SE5","E5_VALOR",17) 		FONT oDlg:oFont ;
                COLOR CLR_HBLUE OF oPanel2 PIXEL
            Else
                @ 004 , 010		Say STR0122    FONT oDlg:oFont OF oPanel2 PIXEL  		// "Quant. Total "
                @ 004 , 042 	Say nQtdTot Picture PesqPict("CTJ","CTJ_QTDTOT",nTamQtd) FONT oDlg:oFont COLOR CLR_HBLUE OF oPanel2 PIXEL
                @ 004 , 130 	Say STR0041	FONT oDlg:oFont OF oPanel2 PIXEL // "Valor T¡tulo: "
                @ 004 , 162 	Say M->E5_VALOR	Picture PesqPict("SE5","E5_VALOR",17) 	FONT oDlg:oFont ;
                COLOR CLR_HBLUE OF oPanel2 PIXEL
            EndIf
            If nOpc <> 5	//Se for Exclusao de titulo nao exibir o valor rateado
                @ 004 , 238 	Say STR0042	FONT oDlg:oFont OF oPanel2 PIXEL 		// "Valor Rateio: "
                @ 004 , 270  	Say oValRat VAR nValRat Picture PesqPict("CTJ","CTJ_VALOR",17)	FONT oDlg:oFont COLOR CLR_HBLUE OF oPanel2 PIXEL
            EndIf

            If lPanelFin  //Chamado pelo Painel Financeiro
                ACTIVATE MSDIALOG oDlg ON INIT (FaMyBar(oDlg,;
                {||nOpca:=1,if(nOpc = 2 .Or. fa050TudCT(nOpc,cPadrao,cProg,nTipo,oGetDB,.T.),oDlg:End(),nOpca := 0)},;
                {||nOpca:=2, fa050DelRat(),oDlg:End()}),oPanel2:Align := CONTROL_ALIGN_BOTTOM)
            Else
                ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar(oDlg,;
                {||nOpca:=1,if(nOpc = 2 .Or. fa050TudCT(nOpc,cPadrao,cProg,nTipo,oGetDB,.T.),oDlg:End(),nOpca := 0)},;
                {||nOpca:=2, fa050DelRat(),oDlg:End()})
            Endif
        Else
            nOpca :=1
        EndIf
    ElseIf cProg == "FINA370"

        nOpca := 1
        nOpc  := If(cPadrao $ "516/517",NIL,nOpc)

    EndIf

    If (nOpca == 1 .And. nOpc <> 2 .And. cPadrao $ "511/512/557/558") .Or. (cPadrao $ "516/517" .And. cProg == "FINA370")
        cSeq := Fa050GerLc( cPadrao,cLote, cPrograma, If(nTipo=2,3,nOpc), @nHdlPrv, @nTotal, @aFlagCTB, cProcPCO, cItemPCO, cRecPag, aRecCV4 )
    Endif

    aRotina := aClone(aRotAnt)
    RestArea(aSaveArea)

Return cSeq

//-------------------------------------------------------
/*/{Protheus.doc} F050HeadCT
Monta aHeader para contabilização

@author Pilar S. Albaladejo.
@since 24.07.00
@version P12
/*/
//-------------------------------------------------------
Function F050HeadCT(cPadrao As Character,;
                    cProg As Character,;
                    aAltera As Array,;
                    nTipo As Numeric)

    Local aSaveArea As Array
    Local aCampos As Array
    Local aFora As Array
    Local lConta As Logical
    Local lCusto As Logical
    Local lItem As Logical
    Local lCLVL As Logical
    Local lMovEnt05 As Logical
    Local lMovEnt06 As Logical
    Local lMovEnt07 As Logical
    Local lMovEnt08 As Logical
    Local lMovEnt09 As Logical
    Local nPos As Numeric
    Local bAdd As Block

    Private nUsado As Numeric

    Default __lF50HEAD  := ExistBlock("F050HEAD")
    Default aAltera := {}

    aSaveArea := GetArea()
    aCampos := {}

    aFora := {"CTJ_FILIAL","CTJ_RATEIO","CTJ_DESC","CTJ_MOEDLC","CTJ_TPSALD","CTJ_SEQUEN","CTJ_QTDTOT"}
    //Se foi escolhido Rateio Digitado ou Pre-configurado, nao ira mostrar o campo de Quantidade Distribuida (CTJ_QTDDIS).
    If nTipo == 1
        Aadd(aFora,"CTJ_QTDDIS")
    EndIf

    lConta := Iif(cPrograma$"FINA050/FINA100" .And. mv_par03==2,.f.,.t.)    // Nao considera contas no rateio -> mv_par03 == 2
    lCusto := CtbMovSaldo("CTT")
    lItem := CtbMovSaldo("CTD")
    lCLVL := CtbMovSaldo("CTH")
    lMovEnt05 := CtbMovSaldo("CT0",,'05')
    lMovEnt06 := CtbMovSaldo("CT0",,'06')
    lMovEnt07 := CtbMovSaldo("CT0",,'07')
    lMovEnt08 := CtbMovSaldo("CT0",,'08')
    lMovEnt09 := CtbMovSaldo("CT0",,'09')

    nPos := 0
    nUsado := 0

    bAdd := {|lAdd,cField1,cField2| If(lAdd,(AAdd(aFora,cField1),AAdd(aFora,cField2)),.F.)}

    // Verificação de quais entidades estão o saldo controlado no contabilidade gerencial
    Eval(bAdd,!lConta,   "CTJ_DEBITO","CTJ_CREDIT")
    Eval(bAdd,!lCusto,   "CTJ_CCD",   "CTJ_CCC"   )
    Eval(bAdd,!lItem,    "CTJ_ITEMD", "CTJ_ITEMC" )
    Eval(bAdd,!lCLVL,    "CTJ_CLVLDB","CTJ_CLVLCR")
    Eval(bAdd,!lMovEnt05,"CTJ_EC05DB","CTJ_EC05CR")
    Eval(bAdd,!lMovEnt06,"CTJ_EC06DB","CTJ_EC06CR")
    Eval(bAdd,!lMovEnt07,"CTJ_EC07DB","CTJ_EC07CR")
    Eval(bAdd,!lMovEnt08,"CTJ_EC08DB","CTJ_EC08CR")
    Eval(bAdd,!lMovEnt09,"CTJ_EC09DB","CTJ_EC09CR")

    aHeader := FinLoadSX3("CTJ",;
                {|cField| Ascan(@aFora,Trim(cField)) <= 0},;
                {   {"X3_TITULO",{|cPar| AllTrim(cPar)}},;
                    {"X3_CAMPO",nil},;
                    {"X3_PICTURE",nil},;
                    {"X3_TAMANHO",nil},;
                    {"X3_DECIMAL",nil},;
                    {"X3_VALID",nil},;
                    {"X3_USADO",nil},;
                    {"X3_TIPO",nil},;
                    {"TMP",nil},;
                    {"X3_CONTEXT",nil}})
    nUsado := Len(aHeader)

    aCampos := FinLoadSX3("CTJ",;
                {|cField| Ascan(@aFora,Trim(cField)) <= 0},;
                {   {"X3_CAMPO",nil},;
                    {"X3_TIPO",nil},;
                    {"X3_TAMANHO",nil},;
                    {"X3_DECIMAL",nil}})
    Aadd(aCampos,{"CTJ_FLAG","L",1,0})

    aAltera := FinLoadSX3("CTJ",;
                {|cField| (Ascan(@aFora,Trim(cField)) <= 0) .And. (Alltrim(cField) <> "CTJ_QTDDIS")},;
                {{"X3_CAMPO",{|cPar| AllTrim(cPar)}}})

    // Carrega validacoes para esta tela
    nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_PERCEN"})
    aHeader[nPos][6] := "CT050CALCP('"+cPadrao+"','"+cProg+"','"+Str(nTipo,1)+"')"
    nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_VALOR"})
    aHeader[nPos][6] := "CT050CALCV('"+cPadrao+"','"+cProg+"','"+Str(nTipo,1)+"')"
    If lConta
        nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_DEBITO"})
        aHeader[nPos][6] := "Vazio() .Or. Ctb105Cta()"
        nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_CREDIT"})
        aHeader[nPos][6] := "Vazio() .Or. Ctb105Cta()"
    EndIf
    If lCusto
        nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_CCD"})
        aHeader[nPos][6] := "Vazio() .Or. Ctb105Cc()"
        nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_CCC"})
        aHeader[nPos][6] := "Vazio() .Or. Ctb105Cc()"
    EndIf

    If lItem
        nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_ITEMD"})
        aHeader[nPos][6] := "Vazio() .Or. Ctb105Item()"
        nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_ITEMC"})
        aHeader[nPos][6] := "Vazio() .Or. Ctb105Item()"
    EndIf

    If lClVl
        nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_CLVLDB"})
        aHeader[nPos][6] := "Vazio() .Or. Ctb105ClVl()"
        nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_CLVLCR"})
        aHeader[nPos][6] := "Vazio() .Or. Ctb105ClVl()"
    EndIf

    If lMovEnt05
        nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_EC05DB"})
        aHeader[nPos][6] := "(Vazio().Or.Ctb120Form())"

        nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_EC05CR"})
        aHeader[nPos][6] := "(Vazio().Or.Ctb120Form())"
    EndIf

    If lMovEnt06
        nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_EC06DB"})
        aHeader[nPos][6] := "(Vazio().Or.Ctb120Form()) "

        nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_EC06CR"})
        aHeader[nPos][6] := "(Vazio().Or.Ctb120Form()) "
    EndIf

    If lMovEnt07
        nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_EC07DB"})
        aHeader[nPos][6] := "(Vazio().Or.Ctb120Form()) "

        nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_EC07CR"})
        aHeader[nPos][6] := "(Vazio().Or.Ctb120Form()) "
    EndIf

    If lMovEnt08
        nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_EC08DB"})
        aHeader[nPos][6] := "(Vazio().Or.Ctb120Form()) "

        nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_EC08CR"})
        aHeader[nPos][6] := "(Vazio().Or.Ctb120Form()) "
    EndIf

    If lMovEnt09
        nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_EC09DB"})
        aHeader[nPos][6] := "(Vazio().Or.Ctb120Form()) "

        nPos := Ascan(aHeader,{|x| Alltrim(x[2]) == "CTJ_EC09CR"})
        aHeader[nPos][6] := "(Vazio().Or.Ctb120Form()) "
    EndIf

    //Ponto de Entrada para inclusao de novos campos.
    If __lF50HEAD
        aCampos := 	ExecBlock("F050HEAD",.f.,.f.,{aCampos})
    EndIf

    RestArea(aSaveArea)

Return aCampos

//-------------------------------------------------------
/*/{Protheus.doc} F050Cria
Cria arquivo temporario para GetDb

@author Pilar S. Albaladejo.
@since 24.07.00
@version P12
/*/
//-------------------------------------------------------
Function F050Cria(aCampos AS Array)

    Local aSaveArea AS Array
    Local nStatus   AS Numeric
    Local lUpdate   AS LOGICAL

    Default __lF50HEAD  := ExistBlock("F050HEAD")

    aSaveArea := GetArea()
    nStatus   := 0
    lUpdate   := SELECT('TMP') == 0

    //-- mv_par03 Considera Contas no Rateio: Consiste estrutura da temporária com o valor configurado
        IF !EMPTY(__oFIN0501) .AND. EMPTY(lUpdate)
            lUpdate := (ASCAN(aCampos,{|e| e[1] == "CTJ_DEBITO"}) == 0) <> ((__oFIN0501:GetAlias())->(FIELDPOS("CTJ_DEBITO")) == 0)
        ENDIF

    //Se existir ponto de entrada para inclusão de novos campos,
    //deletar a tabela para recriar abaixo.
    If (lUpdate .OR. __lF50HEAD) .And. __oFIN0501 <> NIL
        __oFIN0501:Delete()
        __oFIN0501 := NIL
        __cFIN1Name := ""
    EndIf

    //Limpa a tabela temporária no banco, caso já exista
    If(__oFIN0501 <> NIL)
        nStatus := TcSQLExec("DELETE FROM "+__cFIN1Name)                
        FChkTCExec(nStatus, 1 )
    EndIf

    If __oFIN0501 == NIL
        //Cria tabela temporária no banco de dados (alias TMP)
        __oFIN0501 := FwTemporaryTable():New("TMP")
        __oFIN0501:SetFields(aCampos)
        __oFIN0501:AddIndex("1", {Alltrim( aCampos[1][1] )})
        __oFIN0501:Create()

        __cFIN1Name := __oFIN0501:GetRealName()
    EndIf

    dbSelectArea( "TMP" )
    dbSetOrder(0) //ordem natural de inserção

    RestArea(aSaveArea)

Return Nil

//-------------------------------------------------------
/*/{Protheus.doc} F050Carr
Carrega dados para GetDB

@author Pilar S. Albaladejo.
@since 24.07.00
@version P12
/*/
//-------------------------------------------------------
Function F050Carr(nTipo As Numeric,cCodRateio As Character,cProg As Character,cPadrao As Character,cDebito As Character,cCredito As Character,cHistorico As Character,aRecCV4 As Array,aCampos As Array)

    Local aSaveArea	    As Array
    Local cArqRat       As Character
    Local nValor        As Numeric
    Local nRegCTJ       As Numeric
    Local lConta	    As Logical
    Local lCusto	    As Logical
    Local lItem	 	    As Logical
    Local lCLVL	 	    As Logical
    Local nTipoRat  	As Numeric // Tipo de rateio
    Local nCont         As Numeric
    Local nInss		    As Numeric
    Local nRecCTJ       As Numeric
    //Controla o Pis Cofins e Csll na baixa
    Local lPCCBaixa     As Logical

    Local lIRPFBaixa    As Logical
    Local lCalcIssBx    As Logical
    Local nI            As Numeric
    Local nY 	        As Numeric
    Local nX            As Numeric
    Local nZ            As Numeric
    Local nPosCpo       As Numeric
    Local lF50CTMP      As Logical
    Local lF50CTP1      As Logical
    Local lValType      As Logical
    Local nValRateio    As Numeric

    Default aRecCV4 := {}
    Default aCampos := {}


    aSaveArea	:= GetArea()
    lConta	    := Iif(cProg$"FINA050/FINA100" .And. mv_par03==2,.f.,.t.)		// Nao considera contas no rateio -> mv_par03 == 2
    lCusto	    := CtbMovSaldo("CTT")
    lItem	 	:= CtbMovSaldo("CTD")
    lCLVL	 	:= CtbMovSaldo("CTH")
    nTipoRat	:= 1 // Tipo de rateio, 1=Bruto, 2=Liquido
    nCont       := 0
    nInss		:= 0
    //Controla o Pis Cofins e Csll na baixa
    lPCCBaixa   := SuperGetMv("MV_BX10925",.T.,"2") == "1"
    lIRPFBaixa  := IIf( __lLocBRA, SA2->A2_CALCIRF == "2", .F.)
    lCalcIssBx  := IsIssBx("P")
    nI          := 0
    nY 	        := 0
    nX          := 0
    nZ          := 0
    nPosCpo     := 0
    nValRateio  := 0
    lF50CTMP    := ExistBlock("F50CARTMP1")
    lF50CTP1    := ExistBlock("F50CTMP1")
    lValType    := .F.
    
    If cProg == "FINA050" .Or. cPadrao $ "511#512"
        nInss	:= M->E2_INSS
        IF SED->ED_DEDINSS == "2"  //Nao desconta o INSS do principal
            nInss := 0
        Endif

        If cProg == "FINA050"
            nTipoRat	:= mv_par06 // Esta pergunta faz parte do grupo de perguntas do FIN050, mas
            // Nao faz parte do grupo do FIN370 ou AFI100, de onde tambem e
            // utilizada esta funcao, atraves da CtbRatFin.
        Endif
        cArqRat := SE2->E2_ARQRAT
        //soma os impostos da emissao em moeda 1
        nValor := Iif(nTipoRat == 1,;
        If(M->E2_MOEDA > 1,M->E2_VLCRUZ,M->E2_VALOR)+If(lIRPFBaixa,0,M->E2_IRRF)+If(!lCalcIssBx,M->E2_ISS,0)+nInss+M->E2_RETENC+M->E2_SEST+IIF(lPccBaixa,0,M->E2_PIS+M->E2_COFINS+M->E2_CSLL),;
        M->E2_VALOR)

        If nTipoRat == 1 .and. M->E2_MOEDA > 1
            //converte na moeda do titulo
            nValor := Round(NoRound(xMoeda(nValor,1,M->E2_MOEDA,M->E2_EMISSAO,MsDecimais(1)+1,,M->E2_TXMOEDA),MsDecimais(1)+1),MsDecimais(1))
        EndIf
    Else
        nValor	:= M->E5_VALOR
        cArqRat	:= M->E5_ARQRAT
        if Empty(cArqRat)
            cArqRat := FK8->FK8_ARQRAT
        Endif
    EndIf
    nValRat		:= 0
    If nTipo == 1					// Usuario vai digitar o Rateio -> Inclusao
        dbSelectArea("TMP")		// Vale somente para digitacao
        DbGotop()
        If type("aItensCTB")=="A" .and. Len(aItensCTB) > 0 //.and. lF050Auto
            lValType :=  Type("lF050Auto") == "L" .And. lF050Auto
            For nI:= 1 To Len(aItensCTB)
                dbSelectArea("TMP")
                dbAppend()
                For nY:= 1 To Len(aCampos)
                    If ( nPosCpo := Ascan( aItensCTB[nI], { | x | AllTrim(x[ 1 ]) == AllTrim(aCampos[nY,1]) }) ) > 0
                        If lValType .And. __lOtImpMR
                            nX := Ascan(aItensCTB[nI], {|e| AllTrim(e[1]) == "CTJ_PERCEN"})
                            If nX > 0                        
                                nValRateio := (aItensCTB[nI, nX, 2] * M->E2_VALOR ) / 100
                            EndIf

                            nZ := Ascan(aItensCTB[nI], {|e| AllTrim(e[1]) == "CTJ_VALOR"})
                            If nZ > 0 .And. !Empty(nValRateio)
                                aItensCTB[nI,nZ,2] := nValRateio
                            EndIf
                        EndIf
                        TMP->&( Alltrim(aItensCTB[nI,nPosCpo,1]) ) := aItensCTB[nI,nPosCpo,2]
                    EndIf
                Next nY
            Next nI
        EndIf
    ElseIf nTipo == 2				// Rateio ja cadastrado -> Inclusao
        dbSelectArea("CTJ")			// Vale somente para digitacao
        dbSetOrder(1)
        dbSeek(xFilial()+cCodRateio)
        nRegCtj := Recno()
        nQtdTot	:= CTJ->CTJ_QTDTOT
        While !Eof() .And. CTJ->CTJ_FILIAL == xFilial() .And. CTJ->CTJ_RATEIO == cCodRateio
            dbSelectArea("TMP")
            dbAppend()
            For nCont := 1 To Len(aHeader)
                If (aHeader[nCont][08] <> "M" .And. aHeader[nCont][10] <> "V" )
                    // Verifica se o campo existe na estrutura do CTJ
                    TMP->(FieldPut(FieldPos(aHeader[nCont][2]),;
                    (CTJ->(FieldGet(FieldPos(aHeader[nCont][2]))))))
                EndIf
            Next nCont
            TMP->CTJ_FLAG 		:= .F.

            If (! Empty(cDebito) .Or. ! Empty(cCredito)) .And. lConta

                TMP->CTJ_DEBITO := cDebito
                TMP->CTJ_CREDIT := cCredito
                If ! Empty(cDebito)
                    CT1->(MsSeek(xFilial("CT1") + cDebito))
                    If CT1->CT1_ACCUST == "2" .And. lCusto
                        TMP->CTJ_CCD := ""
                    Endif
                    If CT1->CT1_ACITEM == "2" .And. lItem
                        TMP->CTJ_ITEMD	:= ""
                    Endif
                    If CT1->CT1_ACCLVL = "2" .And. lCLVL
                        TMP->CTJ_CLVLDB	:= ""
                    Endif
                Endif

                If ! Empty(cCredito)
                    CT1->(MsSeek(xFilial("CT1") + cCredito))
                    If CT1->CT1_ACCUST == "2" .And. lCusto
                        TMP->CTJ_CCC	:= ""
                    Endif
                    If CT1->CT1_ACITEM == "2" .And. lItem
                        TMP->CTJ_ITEMC	:= ""
                    Endif
                    If CT1->CT1_ACCLVL == "2" .And. lCLVL
                        TMP->CTJ_CLVLCR	:= ""
                    Endif
                Endif
            Endif

            If ! Empty(cHistorico)
                TMP->CTJ_HIST := cHistorico
            Endif
            if TMP->(!EOF())
                TMP->CTJ_VALOR		:= nValor * (TMP->CTJ_PERCEN/100)
            endif
            nValRat += TMP->CTJ_VALOR
            //ponto de entrada para cada linha da CTJ
            If lF50CTMP
                nRecCTJ := CTJ->( Recno() )
                ExecBlock("F50CARTMP1", .F., .F., {cPadrao,nTipo,cProg})
                CTJ->( dbGoto(nRecCTJ) )
            Endif
            dbSelectArea("CTJ")
            dbSkip()
        EndDo
        // Ajusta a diferença do rateio na ultima linha
        if TMP->(!EOF())
            TMP->CTJ_VALOR	+= (nValor - nValRat)
        endif
        nValRat += (nValor - nValRat)
        
        dbSelectArea("CTJ")	
        dbGoto(nRegCTJ)
    ElseIf nTipo == 3		// Exlusao do rateio
        LeDadosCV4(cArqRat,lConta,lCusto,lItem,lClVl,@nValRat, cPadrao, nValor, .T., aRecCV4)
    ElseIf nTipo == 4		//Contabilizacao Off-Line do Rateio
        LeDadosCV4(cArqRat,lConta,lCusto,lItem,lClVl,@nValRat, cPadrao, nValor, .F.)
    ElseIf nTipo == 5					   // Visualizacao do rateio
        LeDadosCV4(cCodRateio,lConta,lCusto,lItem,lClVl,@nValRat, cPadrao, nValor, .F.)
    Endif
    If lF50CTP1
        ExecBlock("F50CTMP1", .F., .F., {cPadrao,nTipo,cProg})
    Endif

    dbSelectArea("TMP")
    dbGoTop()

    RestArea(aSaveArea)

Return nValRat

//-------------------------------------------------------
/*/{Protheus.doc} Ct050CalcP
Calcula o porcentual digitado para rateio no Centro Custo

@author Pilar S. Albaladejo.
@since 15/05/01
@version P12
/*/
//-------------------------------------------------------
Function Ct050CalcP(cPadrao,cProg,cTipo)

    Local aSaveArea	:= GetArea()
    Local lRet			:= .T.
    Local nPercentual := M->CTJ_PERCEN
    Local nReg
    Local nValor
    Local nTipoRat	:= 1 // Tipo de rateio, 1=Bruto, 2=Liquido
    Local nInss := 0
    Local nTotPerc := 0
    //Controla o Pis Cofins e Csll na baixa
    Local lPCCBaixa := SuperGetMv("MV_BX10925",.T.,"2") == "1"
    Local nDecVlr	:= TamSX3("CTJ_VALOR")[2]

    Local lIRPFBaixa := IIf( __lLocBRA, SA2->A2_CALCIRF == "2", .F.)

    Local lCalcIssBx :=	IsIssBx("P")

    If cProg == "FINA050" .Or. cPadrao $ "511#512"
        nInss := M->E2_INSS
        IF SED->ED_DEDINSS == "2"  //Nao desconta o INSS do principal
            nInss := 0
        Endif
        If cProg == "FINA050"
            nTipoRat	:= mv_par06 // Esta pergunta faz parte do grupo de perguntas do FIN050, mas
            // Nao faz parte do grupo do FIN370 ou AFI100, de onde tambem e
            // utilizada esta funcao, atraves da CtbRatFin.
        Endif
        nValor := Iif(nTipoRat == 1,;
        If(M->E2_MOEDA > 1 ,M->E2_VLCRUZ,M->E2_VALOR)+If(lIRPFBaixa,0,M->E2_IRRF)+If(!lCalcIssBx,M->E2_ISS,0)+nInss+M->E2_RETENC+M->E2_SEST+IIF(lPccBaixa,0,M->E2_PIS+M->E2_COFINS+M->E2_CSLL),;
        M->E2_VALOR)

        If nTipoRat == 1 .and. M->E2_MOEDA > 1
            nValor := Round(NoRound(xMoeda(nValor,1,M->E2_MOEDA,M->E2_EMISSAO,MsDecimais(1)+1,,M->E2_TXMOEDA),MsDecimais(1)+1),MsDecimais(1))
        EndIf

    Else
        nValor := M->E5_VALOR
    EndIf

    If nPercentual > 100
        lRet := .F.
    EndIf

    nValRat	:= 0

    If lRet
        IF nPercentual == 0
            TMP->CTJ_PERCEN	:= 0
            TMP->CTJ_VALOR  	:= 0
        Else
            If TMP->(Eof()) .And. FunName() $ "FINA050"
                Reclock("TMP", .T.)
                TMP->CTJ_PERCEN 	:= M->CTJ_PERCEN
                TMP->CTJ_VALOR 	:= Round(NoRound((nValor * nPercentual)/100 ,3),2)
                TMP->(MsUnlock())
            Else
                TMP->CTJ_PERCEN 	:= M->CTJ_PERCEN
                TMP->CTJ_VALOR 	:= Round(NoRound((nValor * nPercentual)/100, nDecVlr + 1), nDecVlr)
            EndIf
        Endif

        //So ira preencher o campo de Quantidade disponivel se o Rateio for pre-configurado.
        If cTipo == "2"
            TMP->CTJ_QTDDIS	:= (TMP->CTJ_PERCEN * nQtdTot) / 100
        EndIf

        nReg := TMP->(Recno())
        TMP->(dbGoTop())
        While	TMP->(!Eof())
            If !TMP->CTJ_FLAG
                nValRat += TMP->CTJ_VALOR
                nTotPerc += TMP->CTJ_PERCEN
            Endif
            TMP->(dbSkip())
        EndDo
        TMP->(dbGoTo(nReg))

        //Acerto de arredondamento
        If nTotPerc == 100 .And. ABS( nValor - nValRat) == 0.01
           
            If (nValor-nValRat) == 0.01
                TMP->CTJ_VALOR += 0.01
                nValRat += 0.01

            ElseIf (nValor-nValRat) == - 0.01
                TMP->CTJ_VALOR -= 0.01
                nValRat -= 0.01
            EndIf
        Endif

    EndIf
    If Type("oValRat")=="O"
        oValRat:Refresh()
    EndIf

    RestArea(aSaveArea)

Return lRet

//-------------------------------------------------------
/*/{Protheus.doc} Ct050CalcV
Calcula o valor digitado para rateio no Centro Custo

@author Pilar S. Albaladejo.
@since 15/05/01
@version P12
/*/
//-------------------------------------------------------
Function Ct050CalcV(cPadrao,cProg,cTipo)

    Local aSaveArea := GetArea()
    Local nValor
    Local nReg
    Local nTipoRat	:= 1 // Tipo de rateio, 1=Bruto, 2=Liquido
    Local nPercCalc := 0
    Local nInss := 0
    //Controla o Pis Cofins e Csll na baixa
    Local lPCCBaixa := SuperGetMv("MV_BX10925",.T.,"2") == "1"
    Local lIRPFBaixa := IIf( __lLocBRA, SA2->A2_CALCIRF == "2", .F.)
    Local lCalcIssBx := IsIssBx("P")

    nValRat := 0

    If TMP->(Eof()) .And. FunName() $ "FINA050"
        Reclock("TMP", .T.)
        TMP->CTJ_VALOR		:= M->CTJ_VALOR
        TMP->(MsUnlock())
    Else
        TMP->CTJ_VALOR		:= M->CTJ_VALOR
    EndIf

    If cProg == "FINA050" .Or. cPadrao $ "511#512"
        nInss := M->E2_INSS
        IF SED->ED_DEDINSS == "2"  //Nao desconta o INSS do principal
            nInss := 0
        Endif

        If cProg == "FINA050"
            nTipoRat	:= mv_par06 // Esta pergunta faz parte do grupo de perguntas do FIN050, mas
            // Nao faz parte do grupo do FIN370 ou AFI100, de onde tambem e
            // utilizada esta funcao, atraves da CtbRatFin.
        Endif
        nValor := Iif(nTipoRat == 1,;
        M->E2_VALOR+If(lIRPFBaixa,0,M->E2_IRRF)+If(!lCalcIssBx,M->E2_ISS,0)+nInss+M->E2_RETENC+M->E2_SEST+IIF(lPccBaixa,0,M->E2_PIS+M->E2_COFINS+M->E2_CSLL),;
        M->E2_VALOR)
    Else
        nValor := M->E5_VALOR
    EndIf

    nPercCalc	:= Round(NoRound((TMP->CTJ_VALOR * 100) / nValor,Max(3,TamSX3("CTJ_PERCEN")[2])),TamSX3("CTJ_PERCEN")[2])

    If nPercCalc > 100 .OR. (TamSX3("CTJ_PERCEN")[2] >= 3 .And. nPercCalc = 100 .And. FunName() $ "FINA050") 			/// Evita erro de replace pois o percentual será maior que o tamanho disponível. (3 inteiros, 2 decimais)
        Return .F.				/// O maior valor permitido para uma linha de rateio será o correspondente a 100% do título/movimento.
    EndIf

    TMP->CTJ_PERCEN	:= nPercCalc
    //So ira preencher o campo de quantidade disponivel se for rateio pre-configurado.
    If cTipo == "2"
        TMP->CTJ_QTDDIS	:= (TMP->CTJ_PERCEN * nQtdTot) / 100
    EndIf

    nReg := TMP->(Recno())
    TMP->(dbGoTop())
    While TMP->(!Eof())
        If !TMP->CTJ_FLAG
            nValRat += TMP->CTJ_VALOR
        EndIf
        TMP->(dbSkip())
    EndDo
    TMP->(dbGoto(nReg))

    If Type("oValRat")=="O"
        oValRat:Refresh()
    EndIf

    RestArea(aSaveArea)

Return .t.

//-------------------------------------------------------
/*/{Protheus.doc} Fa050GerLc
Gera Lancamento Contabil

@author Pilar S. Albaladejo.
@since 15/05/01
@version P12
/*/
//-------------------------------------------------------
Function Fa050GerLc( cPadrao,cLote,cPrograma, nOpc,nHdlPrv,nTotal,aFlagCTB, cProcPCO, cItemPCO, cRecPag, aRecCV4, lUsaFlag )

    Local aSaveArea		:= GetArea()
    Local aTps
    Local aParc
    Local cArquivo
    Local lDigita		:= .F.
    Local lCusto		:= CtbMovSaldo("CTT")
    Local lItem	 		:= CtbMovSaldo("CTD")
    Local lCLVL	 		:= CtbMovSaldo("CTH")
    Local lMovEnt05 	:= CtbMovSaldo("CT0",,'05')
    Local lMovEnt06 	:= CtbMovSaldo("CT0",,'06')
    Local lMovEnt07 	:= CtbMovSaldo("CT0",,'07')
    Local lMovEnt08 	:= CtbMovSaldo("CT0",,'08')
    Local lMovEnt09 	:= CtbMovSaldo("CT0",,'09')

    // Nao considera contas no rateio -> mv_par03 == 2 e programa igual a FINA050,
    // pois no FINA100 e FINA370, a variavel MV_PAR03 se refere a outra pergunta.
    Local lConta		:= Iif(cPrograma $ "FINA050/FINA100"  .And. mv_par03==2,.f.,.t.)
    Local lAglutina		:= .F.
    Local lContabiliza  := .T.
    Local lRateio		:= .T.
    Local nReg			:= 0
    Local nCont
    Local nPis			:= SE2->E2_PIS
    Local nCofins		:= SE2->E2_COFINS
    Local nCsll			:= SE2->E2_CSLL
    Local nMoeda		:= SE2->E2_MOEDA
    Local nInss			:= SE2->E2_INSS
    Local nIrrf			:= SE2->E2_IRRF
    Local nIss			:= SE2->E2_ISS
    Local nSest			:= SE2->E2_SEST
    Local nLinTmp1		:= 0
    Local lItSeqCV4 	:= .T.
    Local nTotPerc		:= 0
    Local nTotVal2		:= 0
    Local nTotVal3		:= 0
    Local nTotVal4		:= 0
    Local nTotVal5		:= 0
    Local nTotVal6		:= 0
    Local nTotVal7		:= 0
    Local nTotSEST		:= 0
    //Controla o Pis Cofins e Csll na baixa
    Local lPCCBaixa := SuperGetMv("MV_BX10925",.T.,"2") == "1"
    //Verifica se o ISS eh retido na baixa
    Local lCalcIssBx :=	IsIssBx("P")
    Local aAreaAux
    Local nUltLin	:=	0
    Local nVldTot	:= 0
    Local aEntCont	:= {} // Entidades Contabeis Adicionais
    Local cLog
    Local oModel
    Local oSubFK5
    Local oSubFKA
    Local aAreaAnt
    Local lNextCV4 := .F.
    Local lNext370 := .F.
    Local cChave   := SE5->E5_ARQRAT
    Local lRndSest  := SuperGetMv("MV_RNDSEST",.F.,.F.)

    Private STRLCTPAD

    Default nHdlPrv := 0
    Default nTotal	 := 0

    DEFAULT lUsaFlag	:= SuperGetMV( "MV_CTBFLAG" , .T. /*lHelp*/, .F. /*cPadrao*/)
    DEFAULT aFlagCTB := {}
    DEFAULT cProcPCO := "000021"
    DEFAULT cItemPCO := "01"
    DEFAULT cRecPag  := "P"
    DEFAULT aRecCV4  := {}

    lF100Auto := If(Type('lF100Auto') == "U", .F.,lF100Auto)

    // Assegura que o usuario nao desistiu do rateio
    If (cPrograma=="FINA050" .And. SE2->E2_RATEIO != "S") .Or.;
    (cPrograma=="FINA100" .And. SE5->E5_RATEIO != "S") .Or.;
    Iif(ProcName(1) $ "CTBRATFIN|FA050AXINC" .And. (lF050Auto .or. lF100Auto),.F.,("_RATEIO" $ Upper(ReadVar()))) .Or.; // Nao grava rateio, quando ainda estiver na validacao do campo. //Aqui AVB Se for a rotina de inclusao do rateio e for rotina automatica nao verifica o campo.
    (FWIsInCallStack("F050EscRat") .And. lF050Auto) //Nao grava o rateio na execucao do cTudoOK na substituicao de titulos via Mensagem Unica
        Return //Fa050GerLc
    Endif

    If cPrograma == "FINA050"
        lAglutina	:= Iif(mv_par07==1,.T.,.F.)
        lDigita 	:= Iif(mv_par01==1,.T.,.F.)
        lContabiliza:= Iif(mv_par04==1 .And. ((!SE2->E2_TIPO $ MVPROVIS) .or. (SE2->E2_TIPO $ MVPROVIS .and. mv_par02 == 1)),.T.,.F.)
        lRateio		:= Iif(mv_par06==1,.T.,.F.)

        //Se for exclusao de titulo rateado na emissão (LP511) e ja contabilizado,
        //Devo contabilizar ainda que Contabiliza On-line = Não (MV_PAR04 == 1)
        If cPadrao == "512" .and. SE2->E2_LA == "S"
            lContabiliza := .T.
        Endif
    ElseIf cPrograma == "FINA100"
        lAglutina	:= Iif(mv_par01==1,.T.,.F.)
        lDigita		:= Iif(mv_par02==1,.T.,.F.)
        lContabiliza:= Iif(mv_par04==1,.T.,.F.)
       	//Se for exclusão de movimento bancário com rateio e já foi contabilizado,
        //Forço a contabilização do título, mesmo que esteja configurado para contabilizar offline (mv_par04 == 2)
        If !lContabiliza .And. nOpc == 5 .And. cPadrao $ "557/558" .And. Alltrim(SE5->E5_LA) == "S"
            lContabiliza := .T.
        EndIf
    ElseIf cPrograma == "FINA370"
        lAglutina	:= Iif(mv_par02==1,.T.,.F.)
        lDigita		:= Iif(mv_par01==1,.T.,.F.)
        lContabiliza:= .T.
    EndIf
    // Restaura perguntas da rotina
    pergunte("FIN050",.F.)

    //Compatibilização com os pontos de bloqueio do SIGAPCO
    If Type("cSeqCv4") == "U"
        nSaveSx8Len := GetSx8Len()
        If nOpc == 3 // Inclusao
            cSeqCv4 := GetSx8Num("CV4", "CV4_SEQUEN")
        Else
            cSeqCv4 := ""
        EndIf
    Endif

    dbSelectArea("CV4")
    dbSetOrder(1)
    DbSeek(cChave)   //cChave jah contem filial

    If cPrograma == "FINA370"
        nValRat := 0
    Else
        //Remonto o valor a ratear
        If lRateio
            nValRat := SE2->(E2_VALOR+E2_IRRF+E2_SEST)
            nValRat += SE2->E2_INSS

            If !lCalcIssBx
                nValRat += SE2->E2_ISS
            Endif

            //Pcc pela emissao
            If !lPccBaixa
                If Empty(SE2->E2_PRETPIS)
                    nValRat += IIF(Empty(SE2->E2_VRETPIS), SE2->E2_PIS , SE2->E2_VRETPIS )
                Endif
                If Empty(SE2->E2_PRETCOF)
                    nValRat += IIF(Empty(SE2->E2_VRETCOF), SE2->E2_COFINS , SE2->E2_VRETCOF )
                Endif
                If Empty(SE2->E2_PRETCSL)
                    nValRat += IIF(Empty(SE2->E2_VRETCSL), SE2->E2_CSLL , SE2->E2_VRETCSL )
                Endif
            Endif
        Else
            nValRat := SE2->E2_VALOR
        Endif
    EndIf

    If Select("TMP") > 0 .And.  TMP->(RecCount()) > 0
        // Se o cabecalho nao foi criado por outra rotina
        If lContabiliza // Contabiliza on-line
            If nHdlPrv <= 0
                // Inicializa Lancamento Contabil
                nHdlPrv := HeadProva( cLote,;
                cPrograma,;
                Substr(cUsuario,7,6),;
                @cArquivo )
            Endif
        Endif

        //Inicia processo do lancamento no Pco somente se for conta a pagar ou receber com rateio
        //movimento bancario deve usar o proprio processo --> 000007
        If cProcPco == "000021"
            PcoIniLan(cProcPCO)
        EndIf
        nUltLin	:=	0
        dbSelectArea("TMP")
        dbGoTop()
        While !Eof()
            If !TMP->CTJ_FLAG
                nUltLin++
            Endif
            If cPrograma == "FINA370"
                // quando vier do fina370, não sabe se o rateio foi pelo valor bruto ou liquido, por isso soma todos os valores
                // rateados para pegar o total
                nValRat += TMP->CTJ_VALOR
            EndIf
            DbSkip()
        Enddo
        dbSelectArea("TMP")
        dbGoTop()
        nLinTmp1 := 0

        If nOpc == 3 // Inclusao
            While (GetSx8Len() > nSaveSx8Len)
                ConfirmSX8()
            End
        Endif
        nCTRLLin := 0

        While !Eof()
            nLinTmp1++
            If !TMP->CTJ_FLAG
                nCTRLLin++
                // Variaveis de Contabilizacao exclusivas do SIGACTB
                Historico:= TMP->CTJ_HIST
                If lCusto
                    CustoD	:= TMP->CTJ_CCD
                    CustoC	:= TMP->CTJ_CCC
                EndIf

                // Variaveis de contabilizacao utilizadas no SIGACON e no SIGACTB
                If lConta					// considera contas no rateio -> mv_par03 == 1
                    Debito	:= TMP->CTJ_DEBITO
                    Credito	:= TMP->CTJ_CREDIT
                EndIF
                If lCusto
                    Custo		:= Iif(!Empty(TMP->CTJ_CCD),TMP->CTJ_CCD,TMP->CTJ_CCC)
                EndIf
                If lItem
                    ItemD		:= TMP->CTJ_ITEMD
                    ItemC		:= TMP->CTJ_ITEMC
                EndIf
                If lCLVL
                    ClvlD		:= TMP->CTJ_CLVLDB
                    ClVlC		:= TMP->CTJ_CLVLCR
                EndIf

                /*
                * Entidades Contábeis Adicionais
                */
                aEntCont := {}
                If lMovEnt05
                    aAdd(aEntCont,{"05",TMP->CTJ_EC05DB,TMP->CTJ_EC05CR})
                    EC05DB := TMP->CTJ_EC05DB
                    EC05CR := TMP->CTJ_EC05CR
                EndIf

                If lMovEnt06
                    aAdd(aEntCont,{"06",TMP->CTJ_EC06DB,TMP->CTJ_EC06CR})
                    EC06DB := TMP->CTJ_EC06DB
                    EC06CR := TMP->CTJ_EC06CR
                EndIf

                If lMovEnt07
                    aAdd(aEntCont,{"07",TMP->CTJ_EC07DB,TMP->CTJ_EC07CR})
                    EC07DB := TMP->CTJ_EC07DB
                    EC07CR := TMP->CTJ_EC07CR
                EndIf

                If lMovEnt08
                    aAdd(aEntCont,{"08",TMP->CTJ_EC08DB,TMP->CTJ_EC08CR})
                    EC08DB := TMP->CTJ_EC08DB
                    EC08CR := TMP->CTJ_EC08CR
                EndIf

                If lMovEnt09
                    aAdd(aEntCont,{"09",TMP->CTJ_EC09DB,TMP->CTJ_EC09CR})
                    EC09DB := TMP->CTJ_EC09DB
                    EC09CR := TMP->CTJ_EC09CR
                EndIf

                If cPadrao == "511" .Or. cPadrao == "512"
                    Valor	:= Round(xMoeda(TMP->CTJ_VALOR,nMoeda,1,SE2->E2_EMISSAO,3,SE2->E2_TXMOEDA),2)
                Else
                    Valor	:= TMP->CTJ_VALOR
                Endif

                VlrInStr 	:= Valor
                Valor2		:= Round(nIrrf		* (Valor / nValRat),2)
                Valor3		:= Round(nInss		* (Valor / nValRat),2)
                Valor4		:= Round(nIss		* (Valor / nValRat),2)
                SEST		:= Iif(lRndSest,Round(nSest		* (Valor / nValRat),2),NoRound(nSest		* (Valor / nValRat),2))

                If cPrograma == "FINA370" .AND. !lPCCBaixa .AND. (cPadrao == "511" .OR. cPadrao == "512")
                    If Empty(SE2->E2_PRETPIS) .AND. Empty(SE2->E2_PRETCOF) .AND. Empty(SE2->E2_PRETCSL)
                        Valor5		:= Round(SE2->E2_VRETPIS * (Valor / nValRat),2)
                        Valor6		:= Round(SE2->E2_VRETCOF * (Valor / nValRat),2)
                        Valor7		:= Round(SE2->E2_VRETCSL * (Valor / nValRat),2)
                    Else
                        Valor5		:= 0
                        Valor6		:= 0
                        Valor7		:= 0
                    Endif
                Else
                    Valor5		:= Round(nPis 		* (Valor / nValRat),2)
                    Valor6		:= Round(nCofins	* (Valor / nValRat),2)
                    Valor7		:= Round(nCsll		* (Valor / nValRat),2)
                Endif
                //Somatorio dos valores para verificacao de arredondamentos
                nTotPerc		+= iif(nMoeda>1,(TMP->CTJ_VALOR / nValRat),(Valor / nValRat))
                nTotVal2		+= VALOR2
                nTotVal3		+= VALOR3
                nTotVal4		+= VALOR4
                nTotVal5		+= VALOR5
                nTotVal6		+= VALOR6
                nTotVal7		+= VALOR7
                nTotSEST		+= SEST
                If nTotVal2 > nIRRF
                    VALOR2	:=	VALOR2 - (nTotVal2- nIRRF)
                    nTotVal2	:=	nIRRF
                Endif
                If nTotVal3 > nInss
                    VALOR3	:=	VALOR3 - (nTotVal3 - nInss)
                    nTotVal3	:=	nInss
                Endif
                If nTotVal4 > nIss
                    VALOR4	:=	VALOR4 - (nTotVal4 - nIss)
                    nTotVal4	:=	nIss
                Endif
                If nTotVal5 > nPis
                    VALOR5	:=	VALOR5 - (nTotVal5 - nPis)
                    nTotVal5	:=	nPis
                Endif
                If nTotVal6 > nCofins
                    VALOR6	:=	VALOR6 - (nTotVal6 - nCofins)
                    nTotVal6	:=	nCofins
                Endif
                If nTotVal7 > nCsll
                    VALOR7	:=	VALOR7 - (nTotVal7 - nCsll)
                    nTotVal7	:=	nCsll
                Endif
                nVldTot += valor
                //Verificacao de aplicacao de arredondamento
                If nCTRLLin == nUltLin
                    Valor2 += (nIrrf - nTotVal2)
                    Valor3 += (nInss - nTotVal3)
                    Valor4 += (nIss - nTotVal4)
                    //Se a chamada for do FINA370, o PCC for gerado na emissao e o titulo reteve o PCC, contabilizar o PCC senao levar zero
                    If cPrograma == "FINA370" .AND. !lPCCBaixa .AND. (cPadrao == "511" .OR. cPadrao == "512")
                        If Empty(SE2->E2_PRETPIS) .AND. Empty(SE2->E2_PRETCOF) .AND. Empty(SE2->E2_PRETCSL)
                            Valor5 += (SE2->E2_VRETPIS - nTotVal5)
                            Valor6 += (SE2->E2_VRETCOF - nTotVal6)
                            Valor7 += (SE2->E2_VRETCSL - nTotVal7)
                        Endif
                    Else
                        Valor5 += (nPis - nTotVal5)
                        Valor6 += (nCofins - nTotVal6)
                        Valor7 += (nCsll - nTotVal7)
                    Endif

                    If (nVldTot - (SE2->E2_VLCRUZ + If(mv_par06==1,(nIrrf + nInss + nIss + nPis + nCofins + nCsll),0))) > 0 .and.;
                    (cPadrao == "511" .Or. cPadrao == "512")
                        Valor -= (nVldTot - SE2->E2_VLCRUZ)
                    Elseif (nVldTot - SE5->E5_VALOR) > 0 .and. cPadrao == "516"
                        Valor -= (nVldTot - SE5->E5_VALOR)
                    Endif
                Endif

                // Retorna chave de busca -> quando utiliza variavel VALOR
                cChaveBusca := CtRelation(cPadrao)

                If cPadrao == "511" .Or. cPadrao == "512"
                    // Desposiciona SE2
                    dbSelectArea("SE2")
                Else
                    // Desposiciona SE5
                    dbSelectArea("SE5")
                EndIf

                If nReg = 0
                    nReg := Recno()
                Endif
                STRLCTPAD := nReg // Disponibiliza o registro do titulo/movimento bancario
                // para ser utilizado no LP para recuperar informacoes
                // do registro contabilizado
                If lContabiliza .And. cPrograma $ "FINA050;FINA370" .And. cPadrao $ "511|512" .And. ;
                    lRateio .And. STR(nOpc,1) $ ('4|5') .And. !Empty(SE2->E2_ARQRAT) .And. !lNextCV4
                    CV4->(MsSeek(Rtrim(SE2->E2_ARQRAT)))
                    lNextCV4 := .T.
                EndIf

                IF lContabiliza .And. cPrograma == "FINA370" .And. cPadrao $ "516"
                    lNext370 := .T.
                Endif

                dbGoBottom()
                dbSkip()

                If lContabiliza  // Contabiliza on-line
                    // Prepara Lancamento Contabil
                    //Contabiliza pela variavel VALOR. Nao necessita de controle de flag.
                    nTotal += DetProva( nHdlPrv,;
                    cPadrao,;
                    cPrograma,;
                    cLote,;
                    /*nLinha*/,;
                    /*lExecuta*/,;
                    /*cCriterio*/,;
                    .T. /*lRateio*/,;
                    cChaveBusca,;
                    /*aCT5*/,;
                    /*lPosiciona*/,;
                    aFlagCTB,;
                    /*aTabRecOri*/,;
                    /*aDadosProva*/ )
                Endif

                If cPadrao == "511" .Or. cPadrao == "512"
                    dbSelectArea("SE2")
                Else
                    dbSelectArea("SE5")
                EndIf
                dbGoto(nReg)
                VlrInStr := Valor

                If nOpc == 3 .and. !TMP->CTJ_FLAG // Inclusao
                    // Grava Rateio digitado ou pre-configurado
                    GravaCv4( cSeqCv4,;
                    dDataBase,;
                    If(lConta,TMP->CTJ_DEBITO,""),;
                    If(lConta, TMP->CTJ_CREDIT,""),;
                    TMP->CTJ_PERCEN,;
                    TMP->CTJ_VALOR,;
                    TMP->CTJ_HIST,;
                    If(lCusto, TMP->CTJ_CCC,""),;
                    If(lCusto, TMP->CTJ_CCD,""),;
                    If(lItem, TMP->CTJ_ITEMD,""),;
                    If(lItem, TMP->CTJ_ITEMC,""),;
                    If(lClVl, TMP->CTJ_CLVLDB,""),;
                    If(lClVl, TMP->CTJ_CLVLCR,""),;
                    If(lItSeqCV4, StrZero(nLinTmp1,Len(CV4->CV4_ITSEQ)), NIL),;
                    cProcPCO, ;
                    cItemPCO, ;
                    cPrograma,;
                    aEntCont ) // Entidade Contabeis Adicionais
                ElseIf nOpc == 5 .Or. nOpc == 6
                    If Len(aRecCV4) > 0 .And. nLinTmp1 <= Len(aRecCV4)
                        aAreaAux := GetArea()
                        dbSelectArea("CV4")
                        dbGoto(aRecCV4[nLinTmp1])
                        PcoDetLan(cProcPco, cItemPco, cPrograma)
                        RestArea(aAreaAux)
                    EndIf
                Endif

            Endif
            dbSelectArea("TMP")
            dbSkip()

            If cPrograma == "FINA370"  .and. cPadrao == "516" .and. lNext370
                CV4->(dbSkip())
            Endif

            If lNextCV4
                CV4->(dbSkip())
            EndIf
        Enddo

        //finaliza processo de lancamento no PCO
        If cProcPco == "000021"
            PcoFinLan(cProcPCO)
        EndIf

        Valor	 := 0
        Valor2 	 := 0
        Valor3	 := 0
        Valor4	 := 0
        Valor5 	 := 0
        Valor6	 := 0
        Valor7	 := 0
        SEST	 := 0
        VlrInStr := 0
        If cPadrao == "511" .Or. cPadrao == "512"
            dbSelectArea("SE2")
        Else
            dbSelectArea("SE5")
        EndIf
        dbGoTo(nReg)
        If lContabiliza .And. !lNextCV4 .And. !lNext370 .OR. ((lNextCV4 .or. lNext370 ) .And. cPrograma $ "FINA050;FINA370" .And. cPadrao $ "511|512|516") // Contabiliza on-line
            // Prepara Lancamento Contabil
            If lUsaFlag  // Armazena em aFlagCTB para atualizar no modulo Contabil
                If cPadrao $ "511#512"
                    aAdd( aFlagCTB, {"E2_LA", "S", "SE2", SE2->( Recno() ), 0, 0, 0} )
                Else
                    aAdd( aFlagCTB, {"E5_LA", "S", "SE5", SE5->( Recno() ), 0, 0, 0} )
                Endif
            Endif
            nTotal += DetProva( nHdlPrv,;
            cPadrao,;
            cPrograma,;
            cLote,;
            /*nLinha*/,;
            /*lExecuta*/,;
            /*cCriterio*/,;
            /*lRateio*/,;
            /*cChaveBusca*/,;
            /*aCT5*/,;
            /*lPosiciona*/,;
            @aFlagCTB,;
            /*aTabRecOri*/,;
            /*aDadosProva*/ )
        Endif
    Endif

    If nTotal > 0  .And. lContabiliza .And. cPrograma != "FINA370" .And. SE2->E2_DESDOBR != "S"

        If  UsaSeqCor()
            aDiario := {}
            If cPadrao == '516' .or. cPadrao == '517'
                aDiario := {{"SE5",SE5->(recno()),SE5->E5_DIACTB,"E5_NODIA","E5_DIACTB"}}
            Else   
                aDiario := {{"SE2",SE2->(recno()),SE2->E2_DIACTB,"E2_NODIA","E2_DIACTB"}}
            EndIf    
        Else
            aDiario := {}
        EndIf
               
        DBSELECTAREA("TMP")

        // Efetiva Lan‡amento Contabil
        cA100Incl( cArquivo,;
        nHdlPrv,;
        3 /*nOpcx*/,;
        cLote,;
        lDigita,;
        lAglutina,;
        /*cOnLine*/,;
        /*dData*/,;
        /*dReproc*/,;
        @aFlagCTB,;
        /*aDadosProva*/,;
        aDiario )
        aFlagCTB := {}  // Limpa o coteudo apos a efetivacao do lancamento

        If !lUsaFlag
            If cPadrao == "511" .Or. cPadrao == "512"
                dbSelectArea("SE2")
                dbGoto(nReg)
                // Atualiza flag de Lan‡amento Contábil
                Reclock("SE2")
                Replace E2_LA With "S"
                aTps := {"TX ","INS","ISS","SES"}
                aParc := {SE2->E2_PARCIR,SE2->E2_PARCINS,SE2->E2_PARCISS,SE2->E2_PARCSES}
                dbSetOrder(1)
                For nCont := 1 to Len(aTps)
                    If Dbseek(xFilial("SE2")+SE2->E2_PREFIXO+SE2->E2_NUM+aParc[nCont]+aTps[nCont])
                        Reclock("SE2")
                        Replace E2_LA With "S"
                    Endif
                    dbGoto(nReg)
                Next nCont
            Else
                dbSelectArea("SE5")
                dbGoto(nReg)
                // Atualiza flag de Lan‡amento Contábil
                aAreaAnt := GetArea()
                oModel :=  FWLoadModel('FINM030')//Mov. Bancario Manual
                oModel:SetOperation( 4 ) //Alteração
                oModel:Activate()
                oSubFKA := oModel:GetModel( "FKADETAIL" )
                oSubFKA:SeekLine( 	{ {"FKA_IDORIG", SE5->E5_IDORIG } } )

                //Dados do movimento
                oSubFK5 := oModel:GetModel( "FK5DETAIL" )
                oSubFK5:SetValue( "FK5_LA", "S" )

                If oModel:VldData()
                    oModel:CommitData()
                    oModel:DeActivate()
                Else

                    cLog := cValToChar(oModel:GetErrorMessage()[4]) + ' - '
                    cLog += cValToChar(oModel:GetErrorMessage()[5]) + ' - '
                    cLog += cValToChar(oModel:GetErrorMessage()[6])
                    Help( ,,"M040VALID",,cLog, 1, 0 )
                Endif
                Restarea(aAreaAnt )
            EndIf
        Endif
    Endif
    If cPrograma == "FINA100"
        Pergunte("AFI100",.F.) // Restaura perguntas do FINA100 para pegar os parametros corretos
    ElseIf cPrograma == "FINA370"
        Pergunte("FIN370",.F.) // Restaura perguntas do FINA370 para pegar os parametros corretos
    EndIf

    RestArea(aSaveArea)

Return xFilial("CV4")+DTOS(dDataBase)+cSeqCv4 //Fa050GerLc

//-------------------------------------------------------
/*/{Protheus.doc} FA050LinCT
Analisa a linha digitada

@author Pilar S. Albaladejo.
@since 15/05/01
@version P12
/*/
//-------------------------------------------------------
Function FA050LinCT()

    Local lRet := .T.
    Local lF050LRCT := ExistBlock("F050LRCT") // Validar a inclusao da linha do rateio on-line

    // Se existir o PE F050LRCT, utiliza o retorno do PE para validar a linha
    If lF050LRCT
        lRet := ExecBlock("F050LRCT", .F., .F. )
    Endif

Return lRet

//-------------------------------------------------------
/*/{Protheus.doc} FA050TudCT
Analisa a tela digitada

@author Pilar S. Albaladejo.
@since 15/05/01
@version P12
/*/
//-------------------------------------------------------
Function FA050TudCT(nOpc As Numeric,;
                    cPadrao As Character,;
                    cPrograma As Character,;
                    nTipo As Numeric,;
                    oGetDB As Object,;
                    lRat05 As Logical) As Logical

    Local aSaveArea As Array
    Local nValor    As Numeric
    Local lRet      As Logical
    Local nTipoRat  As Numeric
    Local nValRat   As Numeric
    Local nInss     As Numeric
    Local nVlrN     As Numeric

    //Controla o Pis Cofins e Csll na baixa
    Local lPCCBaixa  As Logical
    Local lIRPFBaixa As Logical
    Local lCalcIssBx As Logical

    Local lCusto     As Logical
    Local lItem      As Logical
    Local lCLVL      As Logical
    Local nX         As Numeric
    Local aCpos      As Array
    Local lF0502RAT  As Logical
    Local lCtaCC     As Logical
    Local lF050RAT   As Logical
    
    DEFAULT nOpc  := 3
    DEFAULT nTipo := 1
    Default lRat05 := .F.

    aSaveArea := GetArea()
    nValor := 0
    lRet := .T.
    nTipoRat := 1 // Tipo de rateio, 1=Bruto, 2=Liquido
    nValRat := 0
    nInss := 0
    nVlrN := 0
    

    //Controla o Pis Cofins e Csll na baixa
    lPCCBaixa := SuperGetMv("MV_BX10925",.T.,"2") == "1"
    lIRPFBaixa := IIf( __lLocBRA, SA2->A2_CALCIRF == "2", .F.)
    lCalcIssBx := IsIssBx("P")

    lCusto := CtbMovSaldo("CTT")
    lItem := CtbMovSaldo("CTD")
    lCLVL := CtbMovSaldo("CTH")
    nX := 0
    aCpos := {}
    lF0502RAT := ExistBlock("F0502RAT")
    lCtaCC := .F. //Indica se os campos conta debito, conta credito, centro de custo debito e centro de custo credito do arquivo temporario estao em branco.
    lF050RAT := ExistBlock("F050RAT")


    If Select("TMP") <= 0
        Return .T.
    Endif

    If type("n")=="U"  //se a variavel nao existe declara como private
        Private n
    EndIf

    If n != NIL   //salva valor de n utilizado na GetDB
        nVlrN := n
    EndIf

    If cPrograma $ "FINA050/FINA100" .And. Valtype(oGetDB) == "O"
        For nX := 1 TO Len(oGetDB:aAlter)
            If  X3Uso(GetSX3Cache(oGetDB:aAlter[nX],"X3_USADO")) .and.;
                (X3Obrigat(GetSX3Cache(oGetDB:aAlter[nX],"X3_CAMPO")) .or. VerByte(GetSX3Cache(oGetDB:aAlter[nX],"X3_RESERV"),7))
                aAdd(aCpos, Alltrim(oGetDB:aAlter[nX]) )
            EndIf
        Next
    EndIf

    nValRat 	:= 0
    lCtaCC		:= .F.
    dbSelectArea("TMP")
    dbGotop()
    While !Eof()
        If !TMP->CTJ_FLAG
            If Empty(TMP->CTJ_PERCEN) 	//Verifica se existe alguma linha com o percentual zerado
                TMP->CTJ_FLAG := .T.		//Deleto linhas em branco
            Else
                If Len(aCpos) > 0
                    For nX := 1 TO TMP->(FCOUNT())
                        If Ascan(aCpos, Alltrim(TMP->(FieldName(nX)))) > 0 .And. Empty(Tmp->(FieldGet(nX)))
                            MsgAlert(STR0228)//Help(1," ","OBRIGAT",,FieldName(nX),3,0)
                            Return (.F.)
                        EndIf
                    Next
                EndIf
                nValRat += TMP->CTJ_VALOR
            Endif

            If (FUNNAME() $ "FINA050/FINA100/FINA750" .or. lF050Auto ).and. mv_par03 == 2
                If Empty(TMP->CTJ_CCD) .And. Empty(TMP->CTJ_CCC)
                    lCtaCC	:= .T.
                Endif
            Else
                If Empty(TMP->CTJ_CCD) .And. Empty(TMP->CTJ_CCC) .And. Empty(TMP->CTJ_DEBITO) .And. Empty(TMP->CTJ_CREDIT)
                    lCtaCC	:= .T.
                Endif
            EndIf

        EndIf

        dbSkip()
    EndDo
    
  
       
    If lF050RAT
        lRet := ExecBlock("F050RAT",.F.,.F.)
        Return lRet
    EndIf

    //Caso conta debito, conta credito, centro de custo debito e centro de custo credito estejam todos em branco não devo gravar o registro.
    If lCtaCC
        If lF050Auto 
            AutoGRLog(STR0229)
            Return (.F.)
        Else
            MsgAlert(STR0229)
            Return (.F.)
        EndIf
    EndIf

    If cPrograma == "FINA050" .Or. cPadrao $ "511#512"
        nInss := M->E2_INSS
        IF SED->ED_DEDINSS == "2"  //Nao desconta o INSS do principal
            nInss := 0
        Endif

        If cPrograma == "FINA050"
            nTipoRat	:= mv_par06 // Esta pergunta faz parte do grupo de perguntas do FIN050, mas
            // Nao faz parte do grupo do FIN370 ou AFI100, de onde tambem e
            // utilizada esta funcao, atraves da CtbRatFin.
        Endif
        nValor := Iif(nTipoRat==1,;
        If(M->E2_MOEDA > 1 ,M->E2_VLCRUZ,M->E2_VALOR)+If(lIRPFBaixa,0,M->E2_IRRF)+If(!lCalcIssBx,M->E2_ISS,0)+nInss+M->E2_RETENC+M->E2_SEST+IIF(lPccBaixa,0,M->E2_PIS+M->E2_COFINS+M->E2_CSLL),;
        M->E2_VALOR)

        If nTipoRat == 1 .and. M->E2_MOEDA > 1
            nValor := Round(NoRound(xMoeda(nValor,1,M->E2_MOEDA,M->E2_EMISSAO,MsDecimais(1)+1,,M->E2_TXMOEDA),MsDecimais(1)+1),MsDecimais(1))

        EndIf
    Else
        nValor := M->E5_VALOR
    EndIf

    

    
    If nOpc <> 5
        If (Str(nValRat,17,2) != Str(nValor,17,2))  
            Help( " ", 1, "FA050RATEI")
            lRet := .F.
        Endif
    EndIf
    
    //tratamento para o modulo sigapco  // POR TOTAL
    If nOpc == 3
        If lRet .And. cPrograma == "FINA050"
            dbSelectArea("TMP")
            dbGotop()
            n := 0
            While TMP->(!Eof())
                n++  //incrementa variavel n utilizado na pcovldlan (bloqueio)
                If !TMP->CTJ_FLAG
                    lRet := PcoVldLan("000021","01","FINA050")
                    If ! lRet
                        Exit
                    EndIf
                EndIf
                TMP->(dbSkip())
            EndDo
            //Restaura o valor de n -- utilizado na GetDB
            n := nVlrN
        ElseIf lRet .And. cPrograma == "FINA100" .And. lRat05
            dbSelectArea("TMP")
            dbGotop()
            n := 0
            While TMP->(!Eof())
                n++  //incrementa variavel n utilizado na pcovldlan (bloqueio)
                If !TMP->CTJ_FLAG
                    lRet := PcoVldLan("000007","05","FINA100")
                    If !lRet
                        Exit
                    EndIf
                EndIf
                TMP->(dbSkip())
            EndDo
            //Restaura o valor de n -- utilizado na GetDB
            n := nVlrN
        EndIf
    EndIf

    If lRet .and. lF0502RAT
        lRet := ExecBlock("F0502RAT",.F.,.F.)
    EndIf

    // Tipo de Rateio Pre-Configurado deve validar Centro de custo/Item/Classe de Valor informados
    If lRet .And. nTipo == 2
        TMP->( dbGotop() )
        Do While TMP->( !Eof() )
            If !TMP->CTJ_FLAG
                // Verifica centros de custo credito e debito
                If lCusto
                    If !Empty(TMP->CTJ_CCD)
                        lRet := Ctb105CC(TMP->CTJ_CCD)
                        If !lRet
                            Exit
                        EndIf
                    EndIf
                    If !Empty(TMP->CTJ_CCC)
                        lRet := Ctb105CC(TMP->CTJ_CCC)
                        If !lRet
                            Exit
                        EndIf
                    EndIf
                EndIf
                // Verifica item credito e item debito
                If lItem
                    If !Empty(TMP->CTJ_ITEMD)
                        lRet := Ctb105Item(TMP->CTJ_ITEMD)
                        If !lRet
                            Exit
                        EndIf
                    EndIf
                    If !Empty(TMP->CTJ_ITEMC)
                        lRet := Ctb105Item(TMP->CTJ_ITEMC)
                        If !lRet
                            Exit
                        EndIf
                    EndIf
                EndIf
                // Verifica classe de valor credito e debito
                If lCLVL
                    If !Empty(TMP->CTJ_CLVLDB)
                        lRet := Ctb105ClVl(TMP->CTJ_CLVLDB)
                        If !lRet
                            Exit
                        EndIf
                    EndIf
                    If !Empty(TMP->CTJ_CLVLCR)
                        lRet := Ctb105ClVl(TMP->CTJ_CLVLCR)
                        If !lRet
                            Exit
                        EndIf
                    EndIf
                EndIf
            EndIf
            TMP->( dbSkip() )
        EndDo
    EndIf

    RestArea(aSaveArea)

Return lRet

//-------------------------------------------------------
/*/{Protheus.doc} Fa050ValRat
Valida Tela de Rateio - Financeiro

@author Pilar S. Albaladejo.
@since 15/05/01
@version P12
/*/
//-------------------------------------------------------
Function Fa050ValRat(nRadio,cCodRateio, oDlg1, cDebito, cCredito, nOpca)

    Local aSaveArea := GetArea()
    Local lRet := .T.
    Local cTpEntida := ""
    Local lF050VLRAT:= ExistBlock("F050VLRAT")

    nOpca := 1

    If nRadio == 1 .And. ! Empty(cCodRateio)
        nOpca := 0
    EndIf

    If lRet
        If nRadio == 2

            If lF050VLRAT
                nOpca:= ExecBlock("F050VLRAT",.f.,.f., cCodRateio)
            EndIf

            dbSelectArea("CTJ")
            dbSetOrder(1)
            If !dbSeek(xFilial()+cCodRateio)
                Help(" ",1,"FA050RATER")
                nOpca := 0
            EndIf

            If 	oDlg1 # Nil .And. mv_par03==1 .And.;
            CtbDigCta(cCodRateio,,,,, @cTpEntida) .And.;
            ! CtbValCta(cDebito, cCredito, cTpEntida)
                Help(" ",1,"CT9DEBCRED")
                nOpca := 0
            Endif

        EndIf
    EndIF

    If nOpca == 1
        If oDlg1 # Nil
            oDlg1:End()
        EndIf
    Else
        lRet := .F.
    Endif

    RestArea(aSaveArea)

Return lRet


//-------------------------------------------------------
/*/{Protheus.doc} fa050IniS
Funcao para inicializacao dos campos de memoria para
rotina de substituicao

@author Wagner Mobile Costa.
@since 21/09/01
@version P12
/*/
//-------------------------------------------------------
Function fa050IniS() AS Logical

    Local aArea 	    AS Array
    Local aAreaSubs     AS Array
    Local aAltera	    AS Array
    Local nInd          AS Numeric
    Local nRegAtu       AS Numeric
    Local lF050TPRV     AS Logical
    Local lCusto	    AS Logical
    Local lItem	 	    AS Logical
    Local lCLVL	 	    AS Logical
    Local lbIniVal      AS Logical
    Local cTamKeyCV4    AS Character
    Local nTcSql        AS Numeric

    Default __lIntPFS  := SuperGetMv("MV_JURXFIN",.T.,.F.) //Integração do Financeiro com o Juridico(Habilitado = .T.)
    Default __lFa050S  := ExistBlock("FA050S")

    aArea     := GetArea()
    aAreaSubs := {}
    aAltera	  := {}
    nInd      := 0
    nRegAtu   := 0
    lF050TPRV := ExistBlock("F050TPRV")
    lCusto	  := CtbMovSaldo("CTT")
    lItem	  := CtbMovSaldo("CTD")
    lCLVL	  := CtbMovSaldo("CTH")
    lbIniVal   := (Type("bIniciaVal") == "B")
    cTamKeyCV4 := TamSx3("CV4_FILIAL")[1] + TamSx3("CV4_DTSEQ")[1] + TamSx3("CV4_SEQUEN")[1]

    nQtdTit	:= If (Type("nQtdTit") != "N",0,nQtdTit)

    If Type("nValorS") # "U" .And. nValorS # Nil

        If Select("__SE2") == 0
            ChkFile("SE2",.F.,"__SE2")
        Endif

        dbSelectArea("SA2")
        dbSetOrder(1)
        dbSeek(xFilial()+cCodFor+cLojaFor)
        lIRProg := IIf(__lLocBRA,IIf(!Empty(SA2->A2_IRPROG),SA2->A2_IRPROG,"2"),"2")

        If nQtdTit == 1
            M->E2_PREFIXO	:= SE2->E2_PREFIXO
            M->E2_NUM		:= SE2->E2_NUM
            M->E2_NATUREZ	:= SE2->E2_NATUREZ
            M->E2_HIST		:= SE2->E2_HIST
        EndIf

        M->E2_VALOR    := nValorS
        M->E2_TXMOEDA  := SE2->E2_TXMOEDA
        M->E2_VLCRUZ   := Round(NoRound(xMoeda(nValorS,nMoedSubs,1,M->E2_EMISSAO,MsDecimais(1)+1,SE2->E2_TXMOEDA),MsDecimais(1)+1),MsDecimais(1))
        M->E2_FORNECE  := cCodFor
        M->E2_LOJA     := cLojaFor
        M->E2_NOMFOR   := SA2->A2_NREDUZ
        M->E2_MOEDA	   := nMoedSubs
        M->E2_VENCTO   := dDataBase + (SE2->E2_VENCTO - SE2->E2_EMISSAO)
        M->E2_VENCREA  := DataValida(M->E2_VENCTO,.T.)

        If FwIsInCallStack('Fa050Subst')
            M->E2_BASEPIS	:= nValorS
            M->E2_BASECOF	:= nValorS
            M->E2_BASECSL	:= nValorS
            M->E2_BASEISS	:= nValorS
            M->E2_BASEIRF	:= nValorS
            M->E2_BASEINS	:= nValorS

            //Carrega os dados banc rios do fornecedor
            If !Empty(M->E2_FORNECE) .and. !Empty(M->E2_LOJA)
                aFornBco := F050CBCO(M->E2_FORNECE, M->E2_LOJA)
                If !Empty(aFornBco)
                    M->E2_FORBCO	:=	aFornBco[1]
                    M->E2_FORAGE	:=	aFornBco[2]
                    M->E2_FAGEDV	:=	aFornBco[3]
                    M->E2_FORCTA	:=	aFornBco[4]
                    M->E2_FCTADV	:=	aFornBco[5]
                    M->E2_FORMPAG	:=	aFornBco[6]
                Endif
            EndIf

        Endif
        If lF050TPRV
            ExecBlock("F050TPRV",.F.,.F.)
        EndIf

        If nQtdTit == 1
            aAreaSubs:= __SUBS->(GetArea())
            dbSelectArea("__SUBS")
            dbSetOrder(__nOrdOk)    //Ordem por __SUBS->E2_OK
            If __SUBS->(DbSeek(cMarca))
                __SE2->(DbGoTo(__SUBS->NUM_REG))

                If __SE2->E2_RATEIO == "S"
            
                    M->E2_RATEIO	:= __SE2->E2_RATEIO

                    CV4->(dbSetOrder(1))    //CV4_FILIAL, CV4_DTSEQ, CV4_SEQUEN
                    If CV4->(dbSeek(__SE2->E2_ARQRAT))
                        // Caso o arquivo exista, o sistema apaga e reconstroi vazio.
                        // Cria aHeader
                        aCampos := F050CmpRat(@aAltera)

                        If Select("TMP") > 0 .And. !Empty(__cFIN1Name)
                            nTcSql := TcSQLExec("DELETE FROM "+__cFIN1Name)
                            FChkTCExec(nTcSql, 1)
                        EndIf

                        F050Cria(aCampos)

                        While CV4->(!Eof()) .And. CV4->(CV4_FILIAL+DTOS(CV4_DTSEQ)+CV4_SEQUEN) == PADR(__SE2->E2_ARQRAT,cTamKeyCV4)

                            dbSelectArea("TMP")
                            RecLock("TMP",.T.)
                            //-- Arquivo temporario pode ter sido criado sem conta debito/credito por conta do mv_par03 == 2 (Nao considera contas no rateio)
                            If TMP->(FieldPos('CTJ_DEBITO')) > 0
                            TMP->CTJ_DEBITO	:= CV4->CV4_DEBITO
                            TMP->CTJ_CREDIT	:= CV4->CV4_CREDIT
                            EndIf
                            If lCusto
                                TMP->CTJ_CCD := CV4->CV4_CCD
                                TMP->CTJ_CCC := CV4->CV4_CCC
                            EndIf
                            If lItem
                                TMP->CTJ_ITEMD := CV4->CV4_ITEMD
                                TMP->CTJ_ITEMC := CV4->CV4_ITEMC
                            EndIf
                            If lCLVL
                                TMP->CTJ_CLVLDB	:= CV4->CV4_CLVLDB
                                TMP->CTJ_CLVLCR	:= CV4->CV4_CLVLCR
                            EndIf
                            TMP->CTJ_HIST := CV4->CV4_HIST
                            TMP->CTJ_VALOR := CV4->CV4_VALOR
                            TMP->CTJ_PERCEN	:= CV4->CV4_PERCEN
                            TMP->CTJ_FLAG := .F.

                            TMP->(MsUnlock())
                            
                            dbSelectArea("CV4")
                            dbSkip()
                        EndDo
                    Endif
                EndIf
            Endif
            RestArea(aAreaSubs)
        EndIf

        If lbIniVal // Usado caso a bExecuta # NIL
            EVAL(bIniciaVal)// AWR - AVERAGE - 11/08/2003
        EndIf
            
        // Executa um possivel ponto de entrada, neste caso grava o campo desejado no inicializador padrão.
        If __lfa050S
            Execblock("FA050S",.f.,.f.)
        Endif

        //Carrega campos do usuário para serem inicializados na tela de inclusão do titulo - __aIniCpos
        FSubsCpoU() 

        // Validação para desconsiderar o SIGAEIC
        If Len(__aIniCpos) > 0 .And. Select("__SUBS") > 0
            aAreaSubs:= __SUBS->(GetArea())
            If !lbIniVal    // Não executa na integração com SigaEic
                dbSelectArea("__SUBS")
                dbSetOrder(__nOrdOk)    //Ordem por __SUBS->E2_OK
                nRegAtu := Recno()
                If __SUBS->(DbSeek(cMarca))
                    __SE2->(DbGoTo(__SUBS->NUM_REG))
                    // Inicializa array com dados do 1o. registro selecionado p/ substituicao.
                    For nInd:= 1 to Len(__aIniCpos)
                        cCampo := "__SE2->"+Alltrim(__aIniCpos[nInd])
                        &("M->"+__aIniCpos[nInd]) := &cCampo
                    Next
                Endif
                dbSelectArea("__SUBS")
                dbGoto(nRegAtu)
            EndIf
            RestArea(aArea)
            RestArea(aAreaSubs)
        Endif
    Endif

    If __lIntPFS .And. FWIsInCallStack("JURA273") .And. FindFunction("J273LoadVar")
        J273LoadVar()
    EndIf

    RestArea( aArea )
    FwFreeArray( aArea )
    FwFreeArray( __aIniCpos )
    FwFreeArray( aAreaSubs )
    FwFreeArray( aAltera )

    __aIniCpos := {}

Return .T.

//-------------------------------------------------------
/*/{Protheus.doc} F050VldPa
Valida a modalidade SPB do PA

@author Mauricio Pequim Jr.
@since 10/04/02
@version P12
/*/
//-------------------------------------------------------
Function F050VldPa()

    Local lRet      := .T.
    Local nMoeda	:=	0
    Local cNatFor	:= ""
    Local aArea		:= GetARea()
    Local aAreaSA6  := SA6->(GetArea())
    Local aAreaSA2	:= SA2->(GetArea())

	If cPaisLoc == "RUS" .AND. M->E2_TIPO $ MVPAGANT
		SA6->(DBSetOrder(1))
		SA6->(MSSeek(xFilial("SA6") + cBancoAdt + cAgenciaAdt + cNumCon))
		nMoeda := Max(IIF(Type("SA6->A6_MOEDAP")=="U",SA6->A6_MOEDA,;
		                   If(SA6->A6_MOEDAP > 0, SA6->A6_MOEDAP, SA6->A6_MOEDA)),;
					  1)
		If M->E2_CONUNI == "1"
			If nMoeda <> 1
				Help(" ", 1, "FA050MOEDA")
				lRet := .F.
			EndIf
		Else
			If nMoeda <> M->E2_MOEDA
				Help(" ", 1, "FA050MOEDA")
				lRet := .F.
			EndIf
		EndIf
	EndIf

    If lRet .And. !cPaisLoc $ "BRA|BOL|ANG|PER|RUS" .And. M->E2_TIPO $ MVPAGANT
        SA6->(DbSetOrder(1))
        SA6->(MsSeek(xFilial() + cBancoAdt + cAgenciaAdt + cNumCon))
        nMoeda   := Max(IIf(Type("SA6->A6_MOEDAP")=="U",SA6->A6_MOEDA,If(SA6->A6_MOEDAP>0,SA6->A6_MOEDAP,SA6->A6_MOEDA)),1)
        If nMoeda <> M->E2_MOEDA
            Help( " ", 1, "FA050MOEDA")
            lRet	:=	.F.
        Endif
    Endif

    If lRet .And. cPaisLoc $ "ARG|ANG|MEX|COL"
        If MV_PAR05 != 1 .OR. MV_PAR09 != 2 .Or. Empty(cChequeAdt)
            Help( " ", 1, "FA050CHOB",, STR0154, 4, 0 ) // "É obrigatório emissão de cheque para adiantamento e movimentação sem cheque!"
            lRet := .F.
        EndIf
        If cPaisLoc $ "ARG|ANG"
            cNatFor	:= Posicione("SA2", 1, xFilial("SA2") + M->E2_FORNECE + M->E2_LOJA , "A2_NATUREZ" )
            If lRet .And. Empty(cNatFor)
                Help( " ", 1, "FA050FORNA",, STR0155, 4, 0 ) // "É obrigatório a natureza do fornecedor para geracao do cheque"
                lRet := .F.
            EndIf
        EndIf
    EndIf

    If lRet
        lRet := fa050Cheque(cBancoAdt,cAgenciaAdt,cNumCon,cChequeAdt,Iif(cPaisLoc $ "ARG",.F.,.T.))
        If !lRet
            lRet := Fa050DigPa(,@M->E2_MOEDA,.F.)
        Endif
    Endif

    RestArea(aAreaSA2)
    RestArea(aAreaSA6)
    RestArea(aArea)

Return lRet

//-------------------------------------------------------
/*/{Protheus.doc} F050ConVal
Converte o valor dos campos para a moeda escolhida para
apresentacao no MSSelect()

@author Mauricio Pequim Jr.
@since 16/04/02
@version P12
/*/
//-------------------------------------------------------
Function F050ConVal(nMoeda)
    Local nValorCpo := Round(NoRound(xMoeda(E2_SALDO+E2_ACRESC-E2_DECRESC,E2_MOEDA,nMoeda,,3),3),2)
Return nValorCpo

//-------------------------------------------------------
/*/{Protheus.doc} Fa050Rateio
Visualizacao do Rateio de Contas a Pagar

@author Wagner Mobile Costa
@since 04/08/02
@version P12
/*/
//-------------------------------------------------------
Function Fa050Rateio(cAlias AS Character, nReg AS Numeric, nOpc AS Numeric) AS Logical
    Local nTcSql    AS Numeric

    Default __lMetric  := FwLibVersion() >= "20210517"

    RegToMemory("SE2",.F.,.F.)

    If ! Empty(SE2->E2_ARQRAT)

        If __lMetric
            // Metrica de controle de acessos 
            FwCustomMetrics():setSumMetric(Alltrim(ProcName()), "financeiro-protheus_qtd-por-acesso_total", 1)
        Endif

        CtbRatFin("511","FINA050","",5,SE2->E2_ARQRAT,2)
    Else
        Help("",1,	"NoRateio",,	CHR(13)+;
        STR0111 + SE2->E2_NUM + CHR(13),4,0) //"Para o titulo "
    Endif

    // Verifica o arquivo de rateio, e deleta o conteúdo do arquivo temporario
    // para que no proximo rateio seja reutilizado a mesma tabela no banco
    If Select("TMP") > 0 .And. !Empty(__cFIN1Name)
        nTcSql := TcSQLExec("DELETE FROM "+__cFIN1Name)
        FChkTCExec(nTcSql, 1)
    EndIf

Return .T.

//-------------------------------------------------------
/*/{Protheus.doc} GeraParcSe2
Gera parcelas no SE2, baseado nas condicoes de pagamento ou
na quantidade definidade pelo usuario

@author Claudio D. de Souza
@since 14/10/02
@version P12
/*/
//-------------------------------------------------------
Static Function GeraParcSe2(cAlias, lEnd,nHdlPrv,nTotal,cArquivo,nSavRecA2,nSavRec,lUsaFlag,aFlagCTB)
    Local nTamParc		:= TamSx3("E2_PARCELA")[1]
    Local cHistSE2		:= IIf(!Empty(cHistDsd),cHistDsd,SE2->E2_HIST)

    //Alimentando a parcela inicial com o que foi definido no campo E2_PARCELA ou com o conteudo inicial do parametro MV_1DUP
    //Formatando o valor da parcela do titulo originador com o tamanho definido no SX3
    Local cTipoPar		:= IIf(SuperGetMV("MV_1DUP")$"0123456789" .OR. (!Empty(SE2->E2_PARCELA) .AND.;
                            !Upper(AllTrim(SE2->E2_PARCELA)) $ "ABCDEFGHIJKLMNOPQRSTUVXWYZ"),"N","C")
    Local cParcSE2 		:= IIf(cTipoPar == "N",;
                            IIf(Empty(SE2->E2_PARCELA),StrZero(Val(SuperGetMV("MV_1DUP")),nTamParc),SE2->E2_PARCELA),;
                            IIf(Empty(SE2->E2_PARCELA),SuperGetMV("MV_1DUP"),SE2->E2_PARCELA))

    Local nMoedSe2		:= SE2->E2_MOEDA
    Local aCampos		:= {}
    Local nX			:= 0
    Local nI			:= 0
    Local a050Desd		:= {}
    Local lSpbinUse		:= SpbInUse()
    Local cModSpb		:= ""
    Local lAcresc		:= .f.
    Local lDecresc		:= .f.
    Local cPadrao		:= ""
    Local lPadrao		:= .F.
    Local nValSaldo		:= 0
    Local nSomaRateio 	:= 0
    Local nTxMoeda		:= SE2->E2_TXMOEDA
    Local cMultNat		:= ""
    Local cRateio		:= ""
    Local lPCCBaixa		:= SuperGetMv("MV_BX10925",.T.,"2") == "1"
    Local lIRPFBaixa 	:= IIf( __lLocBRA, SA2->A2_CALCIRF == "2", .F.)
    Local lCalcIssBx    := IsIssBx("P")
    Local lFKFNATREN    := IIf(__lLocBRA,FKF->(ColumnPos("FKF_NATREN")) > 0,.F.)

    //Rastreamento
    Local aRastroOri	:= {}
    Local aRastroDes	:= {}
    Local cPrefixo		:= ""
    Local cNum			:= ""
    Local cTipo			:= ""
    Local cFornece		:= ""
    Local cLoja			:= ""
    //Parametro que permite ao usuario utilizar o desdobramento da maneira anterior ao implementado com o rastreamento.
    Local lAtuSldNat 	:= .T.
    //Desdobramento com Imposto
    Local nRecOrig 		:= SE2->(RECNO())
    Local lCalcImp 		:= F050BSIMP(3,7)
    Local aNaoGera 		:= {}
    Local lFa050Des		:= ExistBlock("FA050DES")
    Local lF050DESD 	:= ExistBlock("F050DESD")
    Local lF050DIMP 	:= ExistBlock("F050DESIMP")
    Local lF050GRDS		:= ExistBlock("F050GRDS")
    Local lFa050Par		:= ExistBlock("FA050PAR")
    Local aRet			:= {}
    Local nMCusto       := Val(GetMV("MV_MCUSTO"))
    Local nSomaImp      := 0
    Local nMaxPar       := 0
    Local aFKF          := {}
    Local nProp         := 0
    Local lDesd         := .T.
    Local aRatCC        := {}
    Local aRatAux       := {}
    Local nParcelas     := Len(aParcelas)
    Local lContinua     := .T.
    Local nRateio       := 0
    Local nPosPercen    := 0
    Local nCalc1        As Numeric 
    Local nCalc2        As Numeric  
    Local nValDif       As Numeric   
    Local nValRat       As Numeric
    Local aBkpP         As Array
    Local lPco          As Logical  
    
    PRIVATE lMsErroAuto := .F.

    __lNRasDSD := IF(__lNRasDSD == Nil,SuperGetMV("MV_NRASDSD",.T.,.F.), __lNRasDSD)
    
    nCalc1  := 0
    nCalc2  := 0 
    nValDif := 0  
    nValRat := 0
    aBkpP   := {}
    lPco    := .T.

    dbselectarea("SE2")

    ProcRegua(nParcelas)

    // Carrega em aCampos o conteudo dos campos do SE2
    For nX := 1 To fCount()
        Aadd(aCampos, {FieldName(nX), FieldGet(nX)})
    Next nX

    VALOR := 0
    If lSpbInUse
        cModSpb := SE2->E2_MODSPB
    Endif
    // Apaga registro que originou o desdobramento
    a050Desd := {}
    IF lF050DESD
        a050Desd := ExecBlock( "F050DESD" )
    ENDIF

    //Caso não seja base TOP, mantem o processo antigo
    If __lNRasDSD
        FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
        Reclock("SE2",.F.,.T.)
        dbDelete()
        msUnLock()
    Else
        Reclock("SE2")
        Replace E2_SDACRES With E2_ACRESC
        Replace E2_SDDECRE With E2_DECRESC
        //Incluso novo Status=D (Desdobramento) para que o titulo Pai do desdobramento não seja considerado no calculo de imposto
        Replace E2_STATUS With "D"
        Replace E2_LA With "S"	// Contabilização do Registro Base - Contabiliza pela(s) parcela(s)
        Replace E2_ORIGEM With IIF(Empty(E2_ORIGEM),"FINA050",E2_ORIGEM)
        MsUnlock()
    Endif

    //Dados do titulo principal
    cPrefixo	:= aCampos[Ascan(aCampos,{|e| e[1] == "E2_PREFIXO"})][1]
    cNum		:= aCampos[Ascan(aCampos,{|e| e[1] == "E2_NUM"})][1]
    cTipo		:= aCampos[Ascan(aCampos,{|e| e[1] == "E2_TIPO"})][1]
    cFornece	:= aCampos[Ascan(aCampos,{|e| e[1] == "E2_FORNECE"})][1]
    cLoja		:= aCampos[Ascan(aCampos,{|e| e[1] == "E2_LOJA"})][1]
    cMultNat 	:= aCampos[Ascan(aCampos,{|e| e[1] == "E2_MULTNAT"})][2]
    cRateio  	:= aCampos[Ascan(aCampos,{|e| e[1] == "E2_RATEIO"})][2]

    //Preenche com os dados da FKF posicionada do titulo original
    If __lLocBRA .and. AliasInDic("FKF")
        aFKF := { 	{ "FKF_CPRB"  , FKF->FKF_CPRB     , NIL },;
                    { "FKF_CNAE"  , FKF->FKF_CNAE     , NIL },;
                    { "FKF_TPREPA", FKF->FKF_TPREPA   , NIL },;
                    { "FKF_TPSERV", FKF->FKF_TPSERV   , NIL },;
                    { "FKF_INDDEC", FKF->FKF_INDDEC   , NIL },;
                    { "FKF_INDSUS", FKF->FKF_INDSUS   , NIL }}

        If lFKFNATREN
            Aadd( aFKF ,{"FKF_NATREN",	FKF->FKF_NATREN, NIL })
        Endif                    
    EndIf

    //Rastreamento de titulos em desdobramento
    __cChTitDs := ""
    If !__lNRasDSD
        aAdd(aRastroOri,{E2_FILIAL,;
        E2_PREFIXO,;
        E2_NUM,;
        E2_PARCELA,;
        E2_TIPO,;
        E2_FORNECE,;
        E2_LOJA,;
        E2_VALOR })
        __cChTitDs:= E2_FILIAL + E2_PREFIXO + E2_NUM + E2_PARCELA + E2_TIPO + E2_FORNECE + E2_LOJA
    Endif

    lAcresc	:=	lDecresc := .F.
    If nParcelas == Len(aParcAcre)
        lAcresc := .T.
    Endif
    If nParcelas == Len(aParcDecre)
        lDecresc := .T.
    Endif

    //So deve proporcionalizar quando o valor do titulo for rateado entre as parcelas.
    If MV_MULNATP .And. SE2->E2_MULTNAT == "1" .and. Left(cSE2TpDsd,1) == "T"
        nProp := (SE2->E2_VALOR / nParcelas) / SE2->E2_VALOR
        aEval(aColsSev,{ |e| e[2] *= nProp} ) // Altera o valor conforme a fracao da parcela
        If Select("SEZTMP") > 0
            SEZTMP->(DbGotop())
            While SEZTMP->(!Eof())
                RecLock("SEZTMP",.F.)
                SEZTMP->EZ_VALOR *= 	nProp
                MsUnlock()
                SEZTMP->(DbSkip())
            End
            DbSelectArea("SE2")
        Endif
    Endif

    //Verificacao de conflito de parcela independente da gravacao, para evitar interrupcao na gravacao do desdobramento no meio do processo.
    //Correcao da baixa indevida do titulo, caso o usuario opte pelo cancelamento do desdobramento

    aBkpP := {M->E2_VENCREA,M->E2_VENCTO,M->E2_VALOR,M->E2_DATAAGE,M->E2_EMIS1,M->E2_VLCRUZ,SE2->(RECNO())}
    
    For nI := 1 to nParcelas
        M->E2_VENCREA := DataValida(aParcelas[nI,1],.T.)
        M->E2_VENCTO := aParcelas[nI,1] 
        M->E2_VALOR  := aParcelas[nI,2]
        M->E2_DATAAGE:= DataValida(aParcelas[nI,1],.T.)
        M->E2_EMIS1  := IIf(Type("dDataEmis1") # "U", IIf(!Empty(dDataEmis1),dDataEmis1,dDataBase),dDataBase)
        M->E2_VLCRUZ := Round(NoRound(xMoeda(aParcelas[nI,2],nMoedSE2,1,dDataBase,MsDecimais(1)+1,nTxMoeda),MsDecimais(1)+1),MsDecimais(1))
        
        If PcoVldLan("000002","03","FINA050")
            cParcSE2 := Right("000"+cParcSE2,nTamParc)
            // Para o caso de o titulo que estou incluindo possuir o campo E2_PARCELA preeenchido.
            If Alltrim(M->E2_PARCELA) == Alltrim(cParcSE2)
                cParcSE2 := Soma1(cParcSE2,nTamParc,.T.)
            Endif
            If MsSeek(xFilial("SE2")+&cPrefixo+&cNum+cParcSE2+&cTipo+&cFornece+&cLoja)
                If IW_MsgBox(STR0113 + cParcSe2 + STR0114,STR0115, "YESNO",2) //"Parcela "###" já está cadastrada. Abandona Desdobramento?"###"Atenção"
                    lEnd := .T.
                    Exit
                Else
                    AAdd( aNaoGera, aParcelas[nI,1] )
                Endif
                cParcSE2 := Soma1(cParcSE2,nTamParc,.T.)
            Endif
        else
            lPco := .F.
            lEnd := .T.
            Exit
        endif          
    Next nI

    SE2->(DbGoTo(aBkpP[7]))
    M->E2_VENCREA := aBkpP[1]
    M->E2_VENCTO  := aBkpP[2]
    M->E2_VALOR   := aBkpP[3]
    M->E2_DATAAGE := aBkpP[4]
    M->E2_EMIS1   := aBkpP[5]
    M->E2_VLCRUZ  := aBkpP[6]
    aBkpP := {}

    If !EMPTY(aNaoGera)// No caso do campo Parcela em Branco, busca a ultima parcela para a chave.
        dbSelectArea("SE2")

        cQuery := "select MAX(E2_PARCELA) as ParcMax "
        cQuery += "FROM " + RetSqlName("SE2") + " SE2 "
        cQuery += "WHERE E2_FILIAL = '" + xFilial("SE2") + "' AND "
        cQuery += "E2_PREFIXO ='" +  &cPrefixo + "'AND E2_NUM ='" + &cNum + "'AND "
        cQuery += "E2_TIPO ='" + &cTipo + "' AND E2_FORNECE ='" + &cFornece + "' AND "
        cQuery += "E2_LOJA ='" + &cLoja + "' AND D_E_L_E_T_ = ' '"
        cQuery := ChangeQuery(cQuery)

        dbUseArea(.T., "TOPCONN", TCGenQry(,,cQuery), "QueryParc", .F., .T.)
        nMaxPar:= QueryParc->ParcMax
        cParcSE2 := Soma1(nMaxPar,nTamParc,.T.)
        aNaoGera := {}
        dbSelectArea("SE2")
        QueryParc->(dbCloseArea())
    Endif

    IF lFa050Des
        aRet := ExecBlock("FA050DES",.f.,.f., aParcelas)
        If Valtype(aRet) == "A"
            aParcelas := Aclone(aRet)
        EndIf
    Endif

    If lEnd
        //Voltando o titulo como aberto, ja que o desdobramento foi cancelado
        If !__lNRasDSD
            if lPco
                dbSelectArea(cAlias)
                RecLock(cAlias,.F.)
                Replace E2_SALDO	With E2_VALOR
                Replace E2_BAIXA	With CtoD("//")
                Replace E2_VALLIQ	With 0
                Replace E2_STATUS	With "A"
                Replace E2_FILORIG	With xFilial(cAlias)
                Replace E2_DESDOBR	With "N"
                MsUnlock()
            Else
                FINDELFKs(xFilial("SE2")+"|"+SE2->E2_PREFIXO+"|"+SE2->E2_NUM+"|"+SE2->E2_PARCELA+"|"+SE2->E2_TIPO+"|"+SE2->E2_FORNECE+"|"+SE2->E2_LOJA,"SE2")
                Reclock("SE2",.F.,.T.)
                dbDelete()
                msUnLock()
            endif        
        Endif

        Return
    Else

        If M->E2_RATEIO $ "1S"
            aRatCC := F050RatDsd()
        Endif    
        
        For  nI := 1 to nParcelas
            // Somente gera parcela de desdobramento se passou na validacao anterior
        
            If aScan( aNaoGera, aParcelas[nI,1] ) == 0
                cParcSE2 := Right("000"+cParcSE2,nTamParc)
                While SE2->(DbSeek(xFilial("SE2")+&cPrefixo+&cNum+cParcSE2+&cTipo+&cFornece+&cLoja))
                    //Formatando o valor da parcela de acordo com o seu tamanho para fazer com que a
                    //sequencia dos desdobramentos siga a sequencia da parcela declarada em E2_PARCELA,
                    //independentemente do tamanho utilizado no campo.
                    //Ex: Se parcela for definida como 3, a sequencia sera 04 e nao 31 como estava antes.

                    lDesd:=.T.
                    FOR nX := 1 TO LEN(cParcSE2)
                        IF SUBSTR(cParcSE2,nX,1)$"ZYXWVUTSRQPONMLKJIHGFEDCBA"
                            lDesd:=.F.
                        EndIf
                    NEXT nX
                    
                    If cTipoPar == "N" .And. nTamParc>1 .And. lDesd
                        cParcSE2 := StrZero(Val(cParcSE2),nTamParc)
                    Endif
                    cParcSE2 := Soma1(cParcSE2,nTamParc,.T.)
                EndDo 

                SE2->(dbGoto(nRecOrig))  

                IncProc(STR0112 + cParcSe2) //"Gerando parcela "
                nValSaldo += aParcelas[nI,2]
                //Desdobramento em método novo com rotina Automatica
                If lCalcImp
                    _aTit := {}
                    aRatAux := {}
                    AADD(_aTit , {"E2_PREFIXO",SE2->E2_PREFIXO					,NIL})
                    AADD(_aTit , {"E2_NUM"    ,SE2->E2_NUM						,NIL})
                    AADD(_aTit , {"E2_PARCELA",cParcSE2                      	,NIL})
                    AADD(_aTit , {"E2_TIPO"   ,SE2->E2_TIPO                    	,NIL})
                    AADD(_aTit , {"E2_NATUREZ",SE2->E2_NATUREZ		       		,NIL})
                    AADD(_aTit , {"E2_FORNECE",SE2->E2_FORNECE                 	,NIL})
                    AADD(_aTit , {"E2_LOJA"   ,SE2->E2_LOJA                     ,NIL})
                    AADD(_aTit , {"E2_EMISSAO",SE2->E2_EMISSAO                  ,NIL})
                    AADD(_aTit , {"E2_VENCTO" ,aParcelas[nI,1]         			,NIL})
                    AADD(_aTit , {"E2_VENCREA",DataValida(aParcelas[nI,1],.T.)  ,NIL})
                    AADD(_aTit , {"E2_DATAAGE",DataValida(aParcelas[nI,1],.T.)  ,NIL})
                    AADD(_aTit , {"E2_VENCORI",aParcelas[nI,1]      			,NIL})
                    AADD(_aTit , {"E2_EMIS1"  ,IIf(Type("dDataEmis1") # "U", IIf(!Empty(dDataEmis1),dDataEmis1,dDataBase),dDataBase)	,NIL})
                    AADD(_aTit , {"E2_MOEDA" , SE2->E2_MOEDA                  	,NIL})
                    AADD(_aTit , {"E2_VALOR" , aParcelas[nI,2]                  ,NIL})
                    AADD(_aTit , {"E2_VLCRUZ" ,Round(NoRound(xMoeda(aParcelas[nI,2],nMoedSE2,1,dDataBase,MsDecimais(1)+1,nTxMoeda),MsDecimais(1)+1),MsDecimais(1))	,NIL})
                    AADD(_aTit , {"E2_ORIGEM"  ,"FINA050"                 		,NIL})
                    AADD(_aTit , {"E2_HIST"		,cHistSE2                 		,NIL})
                    If lAcresc
                        AADD(_aTit , {"E2_ACRESC",aParcAcre[nI,2]	, NIL})
                        AADD(_aTit , {"E2_SDACRES",aParcAcre[nI,2]	, NIL})
                    Endif
                    If lDecresc
                        AADD(_aTit , {"E2_DECRESC",aParcDecre[nI,2]	, NIL})
                        AADD(_aTit , {"E2_SDDECRE",aParcDecre[nI,2]	, NIL})
                    Endif
                    If lSpbInUse
                        AADD(_aTit , {"E2_MODSPB",cModSpb, NIL})
                    Endif
                    AADD(_aTit , {"E2_DIRF"		,SE2->E2_DIRF		, NIL})
                    AADD(_aTit , {"E2_CODRET"	,SE2->E2_CODRET		, NIL})

                    aAdd(_aTit,{"AUTCMTIT",aFKF,Nil})

                    If SE2->E2_RATEIO $ "1S"
                        AADD(_aTit , {"E2_RATEIO", SE2->E2_RATEIO, NIL})
                        aRatAux := AClone(aRatCC)
                        nCalc1  := 0
                        nCalc2  := 0 
                        nValDif := 0  
                        nValRat := 0 
                        
                        For nRateio := 1 to Len(aRatAux)
                            
                            nPosPercen := Ascan(aRatAux[nRateio], {|x| x[1] =="CTJ_PERCEN"})
                            nCalc1     := NoRound(aParcelas[nI,2] * (aRatAux[nRateio][nPosPercen]	[2]/100),2)
                            nCalc2     := aParcelas[nI,2] * (aRatAux[nRateio][nPosPercen]	[2]/100) 
                            
                            if nRateio == Len(aRatAux)
                        
                                nValDif += nCalc2
                                nValRat += nCalc1

                                if (nValDif != nValRat) 
                                    if nValDif > nValRat
                                        AADD(aRatAux[nRateio],{"CTJ_VALOR",nCalc1 + (nValDif - nValRat),NIL}) 		  
                                    endif
                                else
                                    AADD(aRatAux[nRateio], {"CTJ_VALOR",nCalc1,NIL})
                                endif 
                            else 
                                AADD(aRatAux[nRateio], {"CTJ_VALOR",nCalc1,NIL})
                                nValRat += nCalc1
                                nValDif += nCalc2
                            endif
                    
                        Next Rateio  
                            
                    Endif
                    
                    If lF050DIMP
                        _aTit := Execblock("F050DESIMP",.F.,.F.,{_aTit})
                    Endif

                    For nX := 1 To Len(aCampos)
                        If aScan(_aTit, { |x| x[1] == aCampos[nX,1]}) <= 0 .And.;
                        !aCampos[nX][1] $ "E2_DESDOBR|E2_BASECOF|E2_BASEPIS|E2_BASECSL|E2_BASEIRF|E2_BASEINS|E2_BASEISS|" //Campos que devem ficar fora do array _aTit
                            aAdd(_aTit,{aCampos[nX][1],aCampos[nX][2],Nil})
                        EndIf
                    Next nX
                    //Chamada da rotina automatica
                    //3 = inclusao
                    MSExecAuto({|x,y,z,a,b,c,d,e,f| FINA050(x,y,z,a,b,c,d,e,f)}, _aTit, 3, 3, /*bExecuta*/, /*aDadosBco*/, /*lExibeLanc*/, /*lOnline*/, aRatAux, /*aTitPrv*/)

                    If lMsErroAuto
                        MOSTRAERRO()
                        DisarmTransaction()
                        lContinua := .F.
                        Exit
                    Endif

                    //Gravacoes complementares
                    RecLock("SE2",.F.)
                    SE2->E2_MULTNAT := cMultNat
                    SE2->E2_DESDOBR := "S"
                    MsUnlock()
                Else

                    RecLock(cAlias,.T.)

                    // Descarrega aCampos no SE2 para que todos os campos preenchidos no titulo principal
                    // sejam replicados aos titulos gerados no desdobramento.
                    For nX := 1 To fCount()
                        If !Empty(aCampos[nX][2])
                            FieldPut(nX,aCampos[nX][2])
                        Endif
                    Next
                    // Grava o restante dos campos que variam conforme a parcela
                    E2_VENCTO 	:= aParcelas[nI,1]
                    E2_VALOR	:= aParcelas[nI,2]
                    E2_PARCELA 	:= cParcSE2
                    E2_HIST    	:= cHistSE2
                    E2_EMIS1	:= IIf(Type("dDataEmis1") # "U", IIf(!Empty(dDataEmis1),dDataEmis1,dDataBase),dDataBase)
                    E2_VENCORI	:= aParcelas[nI,1]
                    E2_SALDO	:= aParcelas[nI,2]
                    E2_ORIGEM  	:= "FINA050"
                    E2_VENCREA 	:= DataValida(aParcelas[nI,1],.T.)
                    E2_DATAAGE 	:= DataValida(aParcelas[nI,1],.T.)
                    E2_VLCRUZ	:= Round(NoRound(xMoeda(aParcelas[nI,2],nMoedSE2,1,dDataBase,MsDecimais(1)+1,nTxMoeda),MsDecimais(1)+1),MsDecimais(1))
                    E2_BASEIRF	:= aParcelas[nI,2]
                    E2_BASEPIS	:= aParcelas[nI,2]
                    E2_BASECOF	:= aParcelas[nI,2]
                    E2_BASECSL	:= aParcelas[nI,2]
                    E2_BASEINS	:= aParcelas[nI,2]
                    E2_BASEISS	:= aParcelas[nI,2]

                    If lAcresc
                        E2_ACRESC  := aParcAcre[nI,2]
                        E2_SDACRES := aParcAcre[nI,2]
                    Endif
                    If lDecresc
                        E2_DECRESC := aParcDecre[nI,2]
                        E2_SDDECRE := aParcDecre[nI,2]
                    Endif
                    If lSpbInUse
                        E2_MODSPB := cModSpb
                    Endif

                    If lAtuSldNat .And. SE2->E2_MULTNAT # "1" .And. SE2->E2_FLUXO == 'S'
                        AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, "2", "P", SE2->E2_VALOR, SE2->E2_VLCRUZ, "+")
                    Endif

                    If SE2->E2_RATEIO $ "1S"  .And. Select("TMP") > 0

                        nSomaRateio := 0
                        // Altera o valor conforme a fracao da parcela
                        TMP->(DbEval({ || TMP->CTJ_VALOR := (TMP->CTJ_PERCEN/100) * SE2->E2_VALOR, nSomaRateio += Round(TMP->CTJ_VALOR,2) })) 
                        TMP->(DbGoBottom())
                        RecLock("TMP")
                        TMP->CTJ_VALOR += (SE2->E2_VALOR - nSomaRateio)
                        MsUnlock()
                        dbSelectArea("SE2")

                    Endif

                    RegToMemory("SE2",.F.,.F.)
                    // Contabiliza o rateio
                    cSeq := Fa050GerLc( "511",cLote, "FINA050", 3, @nHdlPrv, @nTotal )
                    If !Empty(cSeq)
                        RecLock("SE2")
                        Replace E2_ARQRAT	With cSeq
                        If mv_par04 != 2
                            Replace E2_LA	With "S"
                        EndIf
                    EndIf

                    cSeqCv4 := Nil // Para gerar nova numeracao na proxima parcela
                Endif

                IF lfa050Par
                    ExecBlock("FA050PAR",.f.,.f., a050Desd)
                Endif

                //Rastreamento de titulos em desdobramento
                If !__lNRasDSD
                    nSomaImp := IIf( lIRPFBaixa , 0 , SE2->E2_IRRF	)
                    nSomaImp += IIf( lCalcIssBx , 0 , SE2->E2_ISS	)
                    nSomaImp += IIf( lPCCBaixa  , 0 , SE2->E2_VRETPIS	+ SE2->E2_VRETCOF + SE2->E2_VRETCSL)
                    nSomaImp += SE2->E2_INSS

                    aAdd(aRastroDes,{	E2_FILIAL,;
                                        E2_PREFIXO,;
                                        E2_NUM,;
                                        E2_PARCELA,;
                                        E2_TIPO,;
                                        E2_FORNECE,;
                                        E2_LOJA,;
                                        E2_VALOR,;
                                        nSomaImp } )
                Endif

                If MV_MULNATP .And. SE2->E2_MULTNAT == "1"
                    RegToMemory("SE2",.F.,.F.)
                    M->E2_MULTNAT := aCampos[Ascan(aCampos,{|e| e[1] == "E2_MULTNAT"})][2]
                    nSomaRateio := 0
                    aEval(aColsSev, { |e| nSomaRateio += Round(e[2],2) } ) // Soma o valor das multiplas naturezas
                    // Coloca a diferenca entre o valor do titulo e a soma dos rateios na ultima parcela do rateio
                    aColsSev[Len(aColsSev)][2] += (SE2->E2_VALOR - nSomaRateio)
                    If SELECT("SEZTMP") > 0
                        For nX := 1 To Len(aColsSev)
                            If aColsSev[nX][4] == "1"
                                nSomaRateio := 0
                                If SEZTMP->(DbSeek(aColsSev[nX][1]))
                                    While SEZTMP->(!Eof()) .And.;
                                    SEZTMP->EZ_NATUREZ == aColsSev[nX][1]
                                        nSomaRateio +=	Round(SEZTMP->EZ_VALOR,2)
                                        SEZTMP->(DbSkip())
                                    Enddo
                                    SEZTMP->(DbSkip(-1))
                                    RecLock("SEZTMP")
                                    SEZTMP->EZ_VALOR += (Round(aColsSev[nX][2],2) - nSomaRateio)
                                    MsUnlock()
                                Endif
                            Endif
                        Next
                    EndIf
                    dbSelectArea("SE2")
                    GrvSevSez(cAlias,aColsSev,aHeaderSev,,;
                                If(mv_par06 == 1,If(lIRPFBaixa,0,M->E2_IRRF)+If(!lCalcIssBx,M->E2_ISS,0)+ M->E2_INSS +;
                                IIF(lPccBaixa,0,M->E2_PIS+M->E2_COFINS+M->E2_CSLL)+;
                                M->E2_RETENC+M->E2_SEST,0),.F.,"FINA050",mv_par04==1,@nHdlPrv,@nTotal,@cArquivo, .T.)
                Endif
            Endif

            Replace SE2->E2_DESDOBR With If(cRateio != "S" .And. cMultNat != "1","S","N") // Se nao rateia desdobramento
            MsUnlock()
            FKCOMMIT()

            //Gravação do complemento de titulos - FKF
            If __lLocBRA
                Fa986grava("SE2","FINA050")
            EndIf

            // Atualiza Saldos do Fornecedor
            If lF050Auto .or. !lCalcImp
                // Quando possui imposto o saldo é atualizado no A050DupPag
                dbSelectArea("SA2")
                SA2->(dbGoto(nSavRecA2))
                Reclock("SA2" )
                SA2->A2_SALDUP += Round(NoRound(xMoeda(SE2->E2_VALOR,SE2->E2_MOEDA,1,SE2->E2_EMISSAO,3),3),2)
                SA2->A2_SALDUPM+= Round(NoRound(xMoeda(SE2->E2_VALOR,SE2->E2_MOEDA,nMoeda,SE2->E2_EMISSAO,3),3),2)
                If ( SA2->A2_SALDUPM > SA2->A2_MSALDO )
                    SA2->A2_MSALDO := SA2->A2_SALDUPM
                EndIf
                SA2->A2_PRICOM  := Iif(SE2->E2_EMISSAO<A2_PRICOM .Or. Empty(SA2->A2_PRICOM),SE2->E2_EMISSAO,SA2->A2_PRICOM)
                SA2->A2_ULTCOM  := Iif(SA2->A2_ULTCOM<SE2->E2_EMISSAO,SE2->E2_EMISSAO,SA2->A2_ULTCOM)
                SA2->A2_NROCOM  := SA2->A2_NROCOM + 1
                If ( SA2->A2_MCOMPRA < Round(NoRound(xMoeda(SE2->E2_VALOR,SE2->E2_MOEDA,nMCusto,SE2->E2_EMISSAO,3),3),2) )
                    SA2->A2_MCOMPRA := Round(NoRound(xMoeda(SE2->E2_VALOR,SE2->E2_MOEDA,nMCusto,SE2->E2_EMISSAO,3),3),2)
                EndIf
                SA2->A2_MNOTA   := Max(SA2->A2_MNOTA,Round(NoRound(xMoeda(SE2->E2_VALOR,SE2->E2_MOEDA,nMCusto,SE2->E2_EMISSAO,3),3),2) )
                SA2->(MsUnlock())
            EndIf

            // Rotina de contabiliza‡„o do titulo de desdobramento
            IF !E2_TIPO $ MVPROVIS .or. mv_par02 == 1
                If SE2->E2_RATEIO != "S" .And. SE2->E2_MULTNAT != "1" // Se nao rateia desdobramento
                    cPadrao:="577"
                    lPadrao:=VerPadrao(cPadrao)
                    If lPadrao .and. mv_par04 == 1 // Contabiliza On-Line
                        IF nHdlPrv <= 0
                            // Inicializa Lancamento Contabil
                            nHdlPrv := HeadProva( cLote,;
                            "FINA050" /*cPrograma*/,;
                            Substr(cUsuario,7,6),;
                            @cArquivo )
                        Endif
                        If nHdlPrv > 0
                            // Prepara Lancamento Contabil
                            If lUsaFlag  // Armazena em aFlagCTB para atualizar no modulo Contabil
                                aAdd( aFlagCTB, {"E2_LA", "S", "SE2", SE2->( Recno() ), 0, 0, 0} )
                            Endif
                            nTotal += DetProva( nHdlPrv,;
                            cPadrao,;
                            "FINA050" /*cPrograma*/,;
                            cLote,;
                            /*nLinha*/,;
                            /*lExecuta*/,;
                            /*cCriterio*/,;
                            /*lRateio*/,;
                            /*cChaveBusca*/,;
                            /*aCT5*/,;
                            /*lPosiciona*/,;
                            @aFlagCTB,;
                            /*aTabRecOri*/,;
                            /*aDadosProva*/ )
                        Endif

                        If !lUsaFlag
                            // Atualiza flag de Lan‡amento Contábil
                            Reclock("SE2")
                            Replace E2_LA With "S"
                            MsUnLock()
                        Endif

                    Endif
                Else
                    VALOR := 0
                    If SE2->E2_MULTNAT == "1"
                        SEV->(DbGoto(0)) // Desposiciona SEV para contabilizar as demais sequencias do LP 510
                        SEZ->(DbGoto(0)) // Desposiciona SEZ para contabilizar as demais sequencias do LP 510
                        If nHdlPrv <= 0
                            // Inicializa Lancamento Contabil
                            nHdlPrv := HeadProva( cLote,;
                            "FINA050" /*cPrograma*/,;
                            Substr(cUsuario,7,6),;
                            @cArquivo )
                        Endif
                        // Prepara Lancamento Contabil
                        If lUsaFlag  // Armazena em aFlagCTB para atualizar no modulo Contabil
                            aAdd( aFlagCTB, {"E2_LA", "S", "SE2", SE2->( Recno() ), 0, 0, 0} )
                        Endif
                        nTotal += DetProva( nHdlPrv,;
                        If( cRateio == "S", "511", "510" ) /*cPadrao*/,;
                        "FINA050" /*cPrograma*/,;
                        cLote,;
                        /*nLinha*/,;
                        /*lExecuta*/,;
                        /*cCriterio*/,;
                        /*lRateio*/,;
                        /*cChaveBusca*/,;
                        /*aCT5*/,;
                        /*lPosiciona*/,;
                        @aFlagCTB,;
                        /*aTabRecOri*/,;
                        /*aDadosProva*/ )
                    Endif
                Endif
            Endif

            // Grava os lancamentos de desdobramento - SIGAPCO
            PcoDetLan("000002","03","FINA050")

            cParcSE2 := Soma1(cParcSE2,nTamParc,.F.)

            If GetMv("MV_1DUP") == "A"
                While cParcSE2 <> Upper(cParcSE2) .and. SE2->(MsSeek(xFilial("SE2")+&cPrefixo+&cNum+Upper(cParcSE2)+&cTipo))
                    cParcSE2 := Soma1(cParcSE2,nTamParc,.T.)
                EndDo
            EndIf
        Next nI
    EndIf

    If lContinua
        If nTotal > 0
            If SE2->E2_RATEIO != "S" .And. SE2->E2_MULTNAT != "1" // Se nao rateia desdobramento
                dbSelectArea ("SE2")
                dbGoBottom()
                dbSkip()
                VALOR := nValSaldo
                // Prepara Lancamento Contabil
                //Contabiliza pela variavel VALOR. Nao necessita de controle de flag.
                nTotal += DetProva( nHdlPrv,;
                cPadrao,;
                "FINA050" /*cPrograma*/,;
                cLote,;
                /*nLinha*/,;
                /*lExecuta*/,;
                /*cCriterio*/,;
                /*lRateio*/,;
                /*cChaveBusca*/,;
                /*aCT5*/,;
                /*lPosiciona*/,;
                /*@aFlagCTB*/,;
                /*aTabRecOri*/,;
                /*aDadosProva*/ )
                VALOR := 0
            Endif
        Endif

        If lF050GRDS
            ExecBlock("F050GRDS",.F.,.F.)
        EndIf

        //Se existir temporario para rateio c. custo, deleta, pois ele nao foi apagado na rotina que grava o SEV/SEZ para
        // nao prejudicar a gravação do rateio para as demais parcelas
        If Select("SEZTMP") > 0
            FINXDETMP()
        Endif

        // Caso seja um desdobramento, ir  baixar o	titulo gerador do desdobramento
        F050GrvSE5(1,.T.,nSavRec)

        //Gravacao do Rastreamento de titulos em desdobramento FI8
        If !__lNRasDSD
            FINRSTGRV(1,"SE2",aRastroOri,aRastroDes,aRastroOri[1,8])
        Endif
    Endif
    
Return /*GeraParcSe2*/

//-------------------------------------------------------
/*/{Protheus.doc} CtbDigCta
Habilita/Desabilita objetos para digitacao da conta

@cRateio = Codigo do Rateio Externo
@oSayDeb  = Objeto say da digitacao a debito
@oDebito  = Objeto da digitacao da conta a debito
@oSayCrd  = Objeto say da digitacao a credito
@oCredito = Objeto da digitacao da conta a credito
@lRecFoc  = Caso rateio gerencial seta foco no objeto debito

@author Wagner Mobile Costa
@since 06.05.02
@version P12
/*/
//-------------------------------------------------------
Static Function CtbDigCta(	cRateio, oSayDeb, oDebito, oSayCrd, oCredito, cTpEntida, lRecFoc)

    Local lRetGer 	  := cTpEntida <> Nil
    Default cTpEntida := " "
    Default lRecFoc	  := .F.

    CTJ->(DbSeek(xFilial() + cRateio))

    CtjTipoRat("1", @cTpEntida)
    CtjTipoRat("2", @cTpEntida)

    If lRetGer
        Return cTpEntida > "0"
    Endif

    If cTpEntida > "0" .And. mv_par03 = 1
        oSayDeb:Enable()
        oDebito:Enable()
        oSayCrd:Enable()
        oCredito:Enable()
        If lRecFoc
            oDebito:SetFocus()
        Endif
    Else
        oSayDeb:Disable()
        oDebito:Disable()
        oSayCrd:Disable()
        oCredito:Disable()
    Endif

Return .T.

//-------------------------------------------------------
/*/{Protheus.doc} CtjTipoRat
Retorna se eh lancamento do tipo solicitado de acordo  com
o tipo ou as entidades digitadas

@cTipo = Tipo solicitado para retorno
@cTpEntida = Variavel que identifica validacao por entidade

@author Wagner Mobile Costa
@since 06.05.02
@version P12
/*/
//-------------------------------------------------------
Function CtjTipoRat(cTipo, cTpEntida)

    Local lRet := .F.

    If cTipo = "1"
        If ! lRet
            If 	! Empty(CTJ->CTJ_CCD) .And.;
            (Empty(cTpEntida) .Or. cTpEntida = "1") .And.;
            Empty(CTJ->CTJ_DEBITO)
                lRet := .T.
                cTpEntida := "1"
            Endif
            If 	! lRet .And. ! Empty(CTJ->CTJ_ITEMD) .And.;
            (Empty(cTpEntida) .Or. cTpEntida = "2") .And.;
            Empty(CTJ->CTJ_DEBITO)
                lRet := .T.
                cTpEntida := "2"
            Endif
            If 	! lRet .And. ! Empty(CTJ->CTJ_CLVLDB) .And.;
            (Empty(cTpEntida) .Or. cTpEntida = "3") .And.;
            Empty(CTJ->CTJ_DEBITO)
                cTpEntida := "3"
                lRet := .T.
            Endif
        Endif
    ElseIf cTipo = "2"
        If ! lRet
            If 	! Empty(CTJ->CTJ_CCC) .And.;
            (Empty(cTpEntida) .Or. cTpEntida = "1") .And.;
            Empty(CTJ->CTJ_CREDIT)
                lRet := .T.
                cTpEntida := "1"
            Endif
            If 	! lRet .And. ! Empty(CTJ->CTJ_ITEMC) .And.;
            (Empty(cTpEntida) .Or. cTpEntida = "2") .And.;
            Empty(CTJ->CTJ_CREDIT)
                lRet := .T.
                cTpEntida := "2"
            Endif
            If 	! lRet .And. ! Empty(CTJ->CTJ_CLVLCR) .And.;
            (Empty(cTpEntida) .Or. cTpEntida = "3") .And.;
            Empty(CTJ->CTJ_CREDIT)
                lRet := .T.
                cTpEntida := "3"
            Endif
        Endif
    Endif

Return lRet

//-------------------------------------------------------
/*/{Protheus.doc} FA050Bar
Enchoice bar especifica da inclusao de titulos a pagar

@author Mauricio Pequim Jr
@since 15.09.2003
@version P12
/*/
//-------------------------------------------------------
Function FA050Bar(cValidPMS As Character) As Array

    Local aButtons As Array
    Local aUsButtons As Array
    Local lF050BUT	As Logical

    Default __lFinLDCB  := FindFunction("FinLDCB")		

    aButtons   := {}
    aUsButtons := {}
    lF050BUT	:= ExistBlock( "F050BUT" )

    If &(cValidPMS)		// Se usa PMS integrado com o ERP
        AADD(aButtons,{'PROJETPMS',{||Eval(bPmsDlgFI)},STR0094 + " - <F10>",STR0124}) //"Gerenciamento de Projetos"
    Endif

    // Adiciona botoes do usuario na EnchoiceBar
    If lF050BUT
        aUsButtons := ExecBlock( "F050BUT", .F., .F. )
        AEval( aUsButtons, { |x| AAdd( aButtons, x ) } )
    EndIf
    If __lLocBRA
        aAdd(aButtons, {'CONTAINER'   ,{|| FINA986("SE2") },STR0279,STR0279} )//"Complemento do t­tulo"
        If INCLUI .or. ALTERA .and. __lFinLDCB
		    aAdd(aButtons, {'CODBAR', {|| F050CodB()}, STR0331, STR0331}) // "Código de barras"
	    Endif
    EndIf

Return (aButtons)

//-------------------------------------------------------
/*/{Protheus.doc} Fin050del
Altera valores totais quando deleta linha

@author Simone Mie Sato
@since 17.11.03
@version P12
/*/
//-------------------------------------------------------
Function Fin050del()

    Local aSaveArea := GetArea()

    dbSelectArea("TMP")

    If TMP->CTJ_FLAG		// Registro deletado -> eh o contrario pois pressionou <DEL> e o arquivo ainda esta com Flag trocado
        nValRat	+= TMP->CTJ_VALOR
    Else
        nValRat	-= TMP->CTJ_VALOR
    EndIf

    If Type("oValRat")=="O"
        oValRat:Refresh()
    Endif


    RestArea(aSaveArea)

Return .T.


//-------------------------------------------------------
/*/{Protheus.doc} F050VImp
Calcula a data de vencimento de titulos de impostos IR
PIS, COFINS, CSLL

@author Claudio D. de Souza
@since 11/12/03
@version P12
/*/
//-------------------------------------------------------
Function F050VImp(cImposto,dEmissao,dEmis1,dVencRea,cRetencao,cTipoFor,lIRPFBaixa) // Calcula o vencimento do imposto

    Local nK			:= 0
    Local dNextDay 		:= Ctod("//")
    Local nTamData 		:= 0
    Local nNextMes 		:= 0
    Local dDtQuinz 		:= Ctod("//")

    Local lLei11196 	:= SuperGetMv("MV_VC11196",.T.,"2") == "1"
    Local lMP447    	:= SuperGetMV("MV_MP447",.T.,.F.)
    Local nIn480		:= SuperGetMV("MV_IN480",.T.,3)
    Local cVencIRPF 	:= GetMv("MV_VCTIRPF",,"")
    Local lVencIrrf  	:= (SuperGetMv("MV_VENCIRF",.T.,"V") == "V")
    Local lVencPcc  	:= (SuperGetMv("MV_VCPCCP",.T.,1) == 2)
    Local dDtFin790	    := dDataBase

    Local lAntMP351 	:= .F.
    Local lVenctoIN  	:= (SuperGetMv("MV_VENCINS",.T.,"1") == "2")  //1 = Emissao    2= Vencimento Real

    Local lVerIRBaixa	:= .F.
    Local lEmpPublic	:= SuperGetMv("MV_ISPPUBL" ,.T.,"2") == "1"
    Local lINQuinz      := SuperGetMv("MV_IN4815" ,.T.,"Q") == "Q" // Situaçao gerada a partir do chamado SCWLG4, onde foi gerado o boletim tecnico "Vencimento do IRPJ - IN SRF 480"
    Local lVencCRet		:= SuperGetMv("MV_CRTVENC" , .T., "2") == "1"
    Local aAreaSE2		:= GetArea()
    Local nTipo			:= 1
    Local lF050MDVC		:= ExistBlock("F050MDVC")
    Local lFina631		:= IsinCallStack("FINA631")

    Local lCalcIssBx    := IsIssBx("P")
    Local lVcAntIss		:= (SuperGetMV("MV_ANTVISS",.T.,"2") == "1")  //Antecipa ou nao o vencimento do ISS em caso de vencimento em dia nao util
    Local nDiaUtIss		:= SuperGetMv("MV_DIAUISS",.T.,0) //Nro de dias uteis que deve ser gerado o vencimento do titulo do ISS.
    Local nDiaISS       := SuperGetMv("MV_DIAISS",.F.,10,SE2->E2_FILORIG)
    Local cVencIss      := GetNewPar("MV_VENCISS","E")
	Local nMesIss		:= GetNewPar("MV_MESISS" , 1 )
    Local cDiaISS       := StrZero(nDiaIss,2)
    Local dVencRIss		:= CTOD("//")
    Local dVenISS		:= CTOD("//")
    Local nDia          := 0
    Local nDiaUtil      := 0
    Local nI            := 0

    Local cFornSe2		:= SE2->E2_FORNECE
    Local cLojaSe2		:= SE2->E2_LOJA
    Local aAreaFor      := {}

    Default cRetencao 	:= ""
    Default cTipoFor  	:= "J"
    Default cImposto	:= ""
    Default dEmissao 	:= dDataBase
    Default dEmis1		:= dDatabase
    Default dVencRea	:= dDatabase
    Default lIRPFBaixa  := .F.

    lVerIRBaixa := Iif(lIRPFBaixa .AND. cImposto == "IRRF",Iif(cTipoFor == "J",.T.,.F.),.T.) // Verifica se IRPJ na Baixa para calcular vencimento de acordo com a regra do PCC

    If cImposto == "IRRF" .and. !(lEmpPublic .and. cTipoFor == "J" .AND. lIrpfBaixa)
        //Calculo o Vencimento do IR para Pessoa Fisica
        If cTipoFor == "F" .And. !Empty(cVencIRPF)
            If GetMv("MV_VCTIRPF") == "E"
                dNextDay := dEmissao+1
            Elseif GetMv("MV_VCTIRPF") == "C"
                dNextDay := dEmis1+1
            Else
                dNextDay := dVencRea+1
            EndIf
            //Calculo o Vencimento do IR para Pessoa Juridica
        Else
            If GetMv("MV_VENCIRF") == "E"
                dNextDay := dEmissao+1
            Elseif GetMv("MV_VENCIRF") == "C"
                dNextDay := iIf(lFina631, dEmissao+1 , dEmis1+1)
            Else
                dNextDay := dVencRea+1
            EndIf
        EndIf

        //Fato gerador até 31/12/05
        If (!lLei11196 .or. (dNextDay-1) < CTOD("01/01/06")) .and. ;
        !lMP447 .And.;
        !(AllTrim(cRetencao) $ "8739|8767|6147|6175|6190|6188|9060|8850|5706") .AND. ;
        !Empty(cRetencao)

            For nK:=1 To 7
                If Dow( dNextDay ) = 1
                    Exit
                End
                dNextDay++
            Next
            For nK:= 1 to 3
                dNextDay := DataValida(dNextDay+1,.T.)
            Next

        ElseIf AllTrim(cRetencao) $ "8739|8767|6147|6175|6190|6188|9060|8850"

            //Caso seja preenchido com outro valor diferente de 3(3o. dia util) ou 5 (5o. dia util),
            //atribui o valor default 3
            nIn480 := Iif(nIn480 <> 3 .And. nIn480 <> 5,3,nIn480)

            //se aplicar-se o paragrafo II do artigo 5º da IN480, o sistema deverá ir até o final da quinzena
            // para calcular a qtd de dias uteis da semana subsequente.
            If lINQuinz
                dNextDay -= 1 // Retira 1 dia que foi somado
                nTamData := Iif(Len(Dtoc(dNextDay)) == 10, 7, 5)

                If Day(dNextDay) <= 15
                    dNextDay := CTOD("16/"+Subs(Dtoc(dVencrea),4,nTamData))
                Else
                    nNextMes := Month(dNextDay)+1
                    dNextDay := CTOD("01/"+;  //dia
                    Iif(nNextMes==13,"01",StrZero(nNextMes,2))+"/"+; //mes
                    Substr(Str(Iif(nNextMes==13,Year(dNextDay)+1,Year(dNextDay))),2))    //ano
                EndIf
            EndIf

            For nK:=1 To 7
                If Dow( dNextDay ) = 1
                    Exit
                End
                dNextDay++
            Next

            If lINQuinz .and. nIn480 == 5   //ultimo dia útil da semana, se houver somente 4 dias úteis, nao pode cair na semana seguinte
                dNextDay := dNextDay + nIn480
                While DataValida(dNextDay,.T.) <> dNextDay
                    dNextDay := dNextDay - 1
                EndDo
            Else
                For nK:= 1 to nIn480
                    dNextDay := DataValida(dNextDay+1,.T.)
                Next
            EndIf

        ElseIf (AllTrim(cRetencao) $ "5706#9385#8053#3426")

            dNextDay -= 1 // Retira 1 dia que foi somado
            nNextMes := Month(dNextDay)+1

            If Day(dNextDay) >= 1 .And. Day(dNextDay) <= 10 // Primeiro decendio
                //Posiciono no 1o. dia util do decendio subsequente do fato gerador
                dNextDay := CTOD("11/"+StrZero(Month(dNextDay),2)+"/"+Str(Year(dNextDay)))
            ElseIf Day(dNextDay) >= 11 .And. Day(dNextDay) <= 20 // Segundo decendio
                //Posiciono no 1o. dia util do decendio subsequente do fato gerador
                dNextDay := CTOD("21/"+StrZero(Month(dNextDay),2)+"/"+Str(Year(dNextDay)))
            Else //Terceiro decendio
                //Posiciono no 1o. dia util do decendio subsequente do fato gerador
                dNextDay := CTOD("01/"+If(nNextMes==13,"01",StrZero(nNextMes,2))+"/"+;
                Str(If(nNextMes==13,Year(dNextDay)+1,Year(dNextDay))))
            EndIf

            nI := 1
            While nI <= 3
                If DataValida(dNextday,.T.) == dNextDay
                    If nI < 3
                        dNextDay += 1
                    EndIf
                    nI +=1
                Else
                    dNextDay += 1
                Endif
            EndDo

        ElseIf AllTrim(cRetencao) $ SuperGetMv("MV_VENCCRC",,"") //Empresas CRC
            //Calculo da data de vencimento do imposto a partir de 26/07/04 - Lei 10925
            nTamData := Iif(Len(Dtoc(dVencrea)) == 10, 7, 5)

            //Calculo com base na Lei 11196 art. 74
            If Day(dVencRea) <= 15
                dNextDay := Ctod(Str(Day(LastDay(dVencRea)),2)+"/"+Subs(Dtoc(dVencrea),4,nTamData))
            Else
                nNextMes := Month(dVencRea)+1
                dNextDay := CTOD("15/"+Iif(nNextMes==13,"01",StrZero(nNextMes,2))+"/"+;
                Substr(Str(Iif(nNextMes==13,Year(dVencRea)+1,Year(dVencRea))),2))
            Endif

            //Acho o ultimo dia util da semana subsequente
            While .T.
                If DataValida(dNextday,.T.) == dNextDay
                    Exit
                Else
                    dNextDay -= 1
                Endif
            Enddo
            //Media Provisória 447/2008
        ElseIf lMP447 .and. (dNextDay-1) >= CTOD("01/11/08")
            dNextDay -= 1 // Retira 1 dia que foi somado para o calculo anterior.
            //Medida Provisória 447/2008 - Vencimento do IRRF passa a ser no ultimo dia util do segundo decendio
            //do mes subsequente para fatos geradores a partir de 01/11/08
            nNextMes := Month(dNextDay) + 1
            //Monto a data para vigésimo dia do mes subsequente
            dNextDay := CTOD("20/"+Iif(nNextMes==13,"01",StrZero(nNextMes,2))+"/"+ Substr(Str(Iif(nNextMes==13,Year(dNextDay)+1,Year(dNextday))),2))
            //Localiza o ultimo dia util do segundo decenio do mes subsequente
            While .T.
                If DataValida(dNextday,.T.) == dNextDay
                    Exit
                Else
                    dNextDay -= 1
                Endif
            Enddo
        Else
            dNextDay -= 1 // Retira 1 dia que foi somado para o calculo anterior.
            //Lei 11.196 - Vencimento do IRRF passa a ser no ultimo dia util do primeiro decenio do mes seguinte
            //para fatos geradores a partir de 01/01/06
            nNextMes := Month(dNextDay) + 1
            //Monto a data para decimo dia do mes subsequente
            dNextDay := CTOD("10/"+Iif(nNextMes==13,"01",StrZero(nNextMes,2))+"/"+	Substr(Str(Iif(nNextMes==13,Year(dNextDay)+1,Year(dNextday))),2))
            //Acho o ultimo dia util do primeiro decenio do mes subsequente
            While .T.
                If DataValida(dNextday,.T.) == dNextDay
                    Exit
                Else
                    dNextDay -= 1
                Endif
            Enddo
        Endif
    ElseIf cImposto == "FETHAB"
        nDiaVenc := SuperGetMv("MV_VENCFET",.F.,5)
        nAno := Year(dEmissao)

        nMes     := Month(dEmissao)+1
        If nMes > 12
            nMes := 1
            nAno := Year(dEmissao)+1
        Endif

        dData    := CtoD(StrZero(nDiaVenc,2)+"/"+StrZero(nMes,2)+"/"+StrZero(nAno,4))

        If Empty(dData)
            While Empty(dData)
                dData    := CtoD(StrZero(nDiaVenc,2)+"/"+StrZero(nMes,2)+"/"+StrZero(nAno,4))
                nDiaVenc--
            EndDo
        Endif

        dNextDay := DataValida(dData,.T.)

    ElseIf cImposto == "CIDE"

        nNextMes := Month(dEmissao)+1 // Conforme Legislação o Fator Gerador da CIDE é a Emissão.
        dNextDay := CTOD("15/"+Iif(nNextMes==13,"01",StrZero(nNextMes,2))+"/"+;
        Substr(Str(Iif(nNextMes==13,Year(dEmissao)+1,Year(dEmissao))),2))

        dNextDay	:=	DataValida(dNextday,.F.)


    ElseIf cImposto == "INSS"

        //Calculo do Vencto do INSS
        //Por intermedio da Medida Provisoria 351/2007, publicada no DOU 1 de 22.01.2007 (Edicao Extra),
        //foi alterada a data de recolhimento das contribuicoes previdenciarias a cargo da empresa,
        //inclusive as contribuicoes referentes à remuneracao dos empregados, trabalhadores avulsos e a
        //prestacao de servicos do contribuinte individual para o dia 10 do mes seguinte ao da competencia
        //a que se refere.
        If lVenctoIN
            dNextMes := Month(dVencRea)+1
            If dVencRea < CTOD("22/01/07")  //Anterior a MP351
                lAntMP351 := .T.
                dNextVen := CTOD("02/"+IIF(dNextMes==13,"01",StrZero(dNextMes,2))+"/"+;
                Substr(Str(IIF(dNextMes==13,Year(dVencRea)+1,Year(dVencrea))),2))
            Else
                If lMP447 .and. dVencRea > CTOD("01/11/08")
                    //Medida Provisória 447/2008 - Vencimento do INSS passa a ser ate o dia 20
                    //do mes subsequente ao da competencia.
                    //Vencimento para 20
                    lAntMP351 := .F.
                    dNextVen := CTOD("20/"+IIF(dNextMes==13,"01",StrZero(dNextMes,2))+"/"+;
                    Substr(Str(IIF(dNextMes==13,Year(dVencrea)+1,Year(dVencrea))),2))
                Else
                    lAntMP351 := .F.
                    dNextVen := CTOD("10/"+IIF(dNextMes==13,"01",StrZero(dNextMes,2))+"/"+;
                    Substr(Str(IIF(dNextMes==13,Year(dVencrea)+1,Year(dVencrea))),2))
                Endif
            Endif
        Else
            dNextMes := Month(dEmissao)+1
            If dEmissao < CTOD("22/01/07")  //Anterior a MP351
                lAntMP351 := .T.
                dNextVen := CTOD("02/"+IIF(dNextMes==13,"01",StrZero(dNextMes,2))+"/"+;
                Substr(Str(IIF(dNextMes==13,Year(dEmissao)+1,Year(dEmissao))),2))
            Else
                If lMP447 .and. dEmissao > CTOD("01/11/08")
                    //Medida Provisória 447/2008 - Vencimento do INSS passa a ser ate o dia 20
                    //do mes subsequente ao da competencia.
                    //Vencimento para 20
                    lAntMP351 := .F.
                    dNextVen := CTOD("20/"+IIF(dNextMes==13,"01",StrZero(dNextMes,2))+"/"+;
                    Substr(Str(IIF(dNextMes==13,Year(dEmissao)+1,Year(dEmissao))),2))
                Else
                    lAntMP351 := .F.
                    dNextVen := CTOD("10/"+IIF(dNextMes==13,"01",StrZero(dNextMes,2))+"/"+;
                    Substr(Str(IIF(dNextMes==13,Year(dEmissao)+1,Year(dEmissao))),2))
                Endif
            Endif
        Endif

        If lMP447
            // Caso seja pessoa física e FUNRURAL a data de vencimento será prorrogada
            If cTipoFor == "F" .AND. isFunrural()

                While .T.
                    If DataValida(dNextVen,.T.) == dNextVen
                        dNextDay := dNextVen
                        Exit
                    Else
                        dNextVen += 1
                    Endif
                Enddo

            Else
                //Caso o dia do vencimento não for util, será considerado antecipado o prazo para o primeiro
                //dia util que o anteceder.
                While .T.
                    If DataValida(dNextVen,.T.) == dNextVen
                        dNextDay := dNextVen
                        Exit
                    Else
                        dNextVen -= 1
                    Endif
                Enddo
            Endif
        Else
            dNextDay := DataValida(dNextVen,.T.)
        Endif

    ElseIf cImposto == 'ISS'
       	dVenISS := IF(!Empty(SE2->E2_VENCISS), SE2->E2_VENCISS, dVenISS) 
		nMesIss	:= If( nMesIss > 0, nMesIss, 1 )

		If Empty(dVenISS)
			Do Case
				Case nDiaUtIss > 0 //Vencimento do ISS deve ser gerado por dias uteis.

					nMesIss		:= If( nMesIss == 1, 0, nMesIss - 1 )
					
					If lCalcIssBx // Mes subsequente de acordo com o momento de retenção do imposto.
						dVenISS	:= LastDay( MonthSum( dVencRea, nMesIss ) )
					Else
						dVenISS	:= LastDay( MonthSum( dEmissao, nMesIss ) )
					Endif

					For nI:= 1 To nDiaUtIss
						dVenISS ++
						dVenISS := DataValida(dVenISS)
					Next nI

				Case cVencIss == "E" //E=Emissão

                    If !Empty( SE2->E2_CODISS ) .and. AliasInDic("CC2")
                        FIM->( DbSetOrder( 1 ) )
                        If FIM->( DbSeek( xFilial( "FIM" ) + SE2->E2_CODISS ) )
                            CC2->(DbSetOrder( 1 ))
                            If CC2->(DbSeek(xFilial("CC2") + FIM->FIM_EST + FIM->FIM_CODMUN))
                                cDiaISS := StrZero( CC2->CC2_DTRECO, 2 )
                            EndIf
                        EndIf
                    EndIf

					dVenISS		:= MonthSum( dEmissao, nMesIss )
					nTamData	:= If( Len( Dtoc( dVenISS ) ) == 10, 7, 5 )

					//Caso o mes do vcto seja Fevereiro e o parametro MV_DIAISS estiver 30 ou 31
					If Month(dVenISS) == 2 .And. cDiaISS $ "30/31"
						dVenISS := LastDay(dVenISS)
					Else
						dVenISS	:= Ctod( cDiaISS + "/" + Subs( Dtoc( dVenISS ), 4, nTamData ) )
					Endif

				Case cVencIss == "Q" //Ultimo dia util da quinzena subsequente a dEmissao
					
					If Day(dEmissao) <= 15
						nMesIss	:= If( nMesIss == 1, 0, nMesIss - 1 )
						dVenISS	:= LastDay( MonthSum( dEmissao, nMesIss ) )
						dVenISS := DataValida( dVenISS, .F. )
					Else
						//dVenISS := DataValida( ( LastDay(dEmissao) + 1 ) + 14, .F. )
						dVenISS := DataValida( FirstDate( MonthSum( dEmissao, nMesIss ) ) + 14, .F. )
					EndIf

				Case cVencIss == "U" //Ultimo dia util do mes subsequente da dEmissao

					dVenISS := DataValida(LastDay(LastDay(dEmissao)+1),!lVcAntIss)
					
				Case cVencIss == "D"
					
					dVenISS		:= FirstDay( MonthSum( dEmissao, nMesIss ) )
					nDiaUtil	:= nDiaISS
					
					For nDia := 1 To nDiaUtil-1
						If !(dVenISS == DataValida(dVenISS,.T.))
							nDia-=1
						EndIf
						dVenISS+=1
					Next nDia

				Case cVencIss == "F" //Qtd de dia do parametro MV_DIAISS apos o fechamento da quinzena.
					/*F =	Se a data de emissão for menor que 15 (primeira quinzena) a data 
							de vencimento será 15 mais o conteúdo do MV_DIAISS (considerando 
							o primeiro dia do mês na conta), se a data de emissão for maior que 
							15 (segunda quinzena) será no dia informado no MV_DIAISS do mês subsequente.*/

					If Day(dEmissao) <= 15
						nMesIss	:= If( nMesIss == 1, 0, nMesIss - 1 )
						dVenISS := CtoD("15" + SUBSTR( DtoC( MonthSum( dEmissao, nMesIss ) ), 3, Len( DtoC( dEmissao ) ) ) ) + nDiaISS
					Else
						nMesIss	:= If( nMesIss == 1, 0, nMesIss - 1 )
						dVenISS := LastDay( MonthSum( dEmissao, nMesIss ) ) + nDiaISS
					EndIf

				OtherWise //V=Vencimento
					//Ok
					dVenISS		:= MonthSum( dVencRea, nMesIss )
					nTamData	:= If( Len( Dtoc(dVenISS) ) == 10, 7, 5 )
					dVenISS		:= Ctod( cDiaISS + "/" + Subs( Dtoc(dVenISS), 4, nTamData ) )

			EndCase
		EndIf

		dVencRIss := DataValida(dVenISS,IIF(lVcAntIss,.F.,.T.))
		dVenISS := IIF(dVenIss > dVencRIss, dVencRISS, dVenIss)

		If Alltrim(SM0->M0_ESTENT) == "SC" .And. ;
			( ( Len( Alltrim( SM0->M0_CODMUN ) ) == 5 .And. Alltrim( SM0->M0_CODMUN ) == "09102" ) .Or. ( Len( Alltrim( SM0->M0_CODMUN ) ) == 7 .And. SubStr( Alltrim( SM0->M0_CODMUN ) , 3 , 5 ) == "09102" ) )
			aAreaFor:= getArea("SA2")
			DbSelectArea("SA2")
			DbSetOrder(1)
			If SA2->(DbSeek(xFilial("SA2")+cFornSe2+cLojaSe2)) .And. Alltrim(SA2->A2_EST) == "SC"
				dVenISS := fCRetCal(6,dEmissao)
				dVencRIss := dVenISS
			EndIf
			restArea(aAreaFor)
		EndIf

		dNextDay := DataValida(dVenISS,.T.)

    ElseIf !Empty(cImposto)
        If dVencrea < CTOD("16/06/15") .Or. lEmpPublic
            //Calculo da data de vencimento para titulos de PIS, COFINS e CSLL
            //Para o IR na Baixa, segue o mesmo conceito do PCC para o calculo.
            //Verifico se a baixa ou vencimento sao anteriores a Lei 10925 e
            //fato o calculo da data na forma antiga
            If dVencrea < SuperGetMv("MV_RF10925",.t.,CTOD("26/07/04"))
                dNextDay := dVencRea+1
                For nK:=1 To 7
                    If Dow( dNextDay ) = 1
                        Exit
                    Endif
                    dNextDay++
                Next
                For nK:= 1 to 3
                    dNextDay := DataValida(dNextDay+1,.T.)
                Next
            Else

                //Calculo da data de vencimento do imposto a partir de 26/07/04 - Lei 10925
                nTamData := Iif(Len(Dtoc(dVencrea)) == 10, 7, 5)

                //Lei 11.196 - Vencimento do PIS COFINS e CSLL passa a ser no ultimo dia util da quinzena subsequente
                //para fatos geradores a partir de 01/01/06
                //Art. 74 que altera o art.35 da Lei 10833
                //Alterada pela MP 351 de 21/01/07, art 7 e sequintes:
                // O pagamento da Contribuição para o PIS/PASEP e da COFINS deverá ser efetuado ate o ultimo dia util do
                // segundo decendio subsequente ao mes de ocorrencia dos fatos geradores."


                //Calculo antigo para fatos geradores anteriores a vigencia da Lei ou para onde não se aplique
                If lVerIRBaixa .AND. (!lLei11196 .or. dVencRea < CTOD("01/01/06"))
                    //Verifico a quizena do vencimento
                    If Day(dVencRea) <= 15
                        dDtQuinz := Ctod("15/"+Subs(Dtoc(dVencrea),4,nTamData))
                        If Dow(dDtQuinz) == 1   //Se o dia 15 for domingo
                            dNextDay := Ctod("27/"+Subs(Dtoc(dVencrea),4,nTamData))
                        Else
                            dNextDay := Ctod("21/"+Subs(Dtoc(dVencrea),4,nTamData))
                        Endif
                    Else
                        nNextMes := Month(dVencRea)+1
                        dDtQuinz := Ctod(Str(Day(LastDay(dVencRea)),2)+"/"+Subs(Dtoc(dVencrea),4,nTamData))
                        If Dow(dDtQuinz) == 1   //Se o ultimo dia do mes for domingo
                            dNextDay := CTOD("12/"+Iif(nNextMes==13,"01",StrZero(nNextMes,2))+"/"+;
                            Substr(Str(Iif(nNextMes==13,Year(dVencRea)+1,Year(dVencRea))),2))
                        Else
                            dNextDay := CTOD("06/"+Iif(nNextMes==13,"01",StrZero(nNextMes,2))+"/"+;
                            Substr(Str(Iif(nNextMes==13,Year(dVencRea)+1,Year(dVencRea))),2))
                        Endif
                    Endif

                    //Acho a Sexta feira da semana subsequente
                    nDiaSemana := Dow(dNextDay)
                    If nDiaSemana < 6
                        dNextDay += 6-nDiaSemana
                    ElseIf nDiaSemana > 6
                        dNextDay -= 1
                    Endif
                ElseIf lLei11196

                    //Calculo com base na Lei 11196 art. 74
                    If Day(dVencRea) <= 15
                        dNextDay := Ctod(Str(Day(LastDay(dVencRea)),2)+"/"+Subs(Dtoc(dVencrea),4,nTamData))

                    Else
                        nNextMes := Month(dVencRea)+1
                        dNextDay := CTOD("15/"+Iif(nNextMes==13,"01",StrZero(nNextMes,2))+"/"+;
                        Substr(Str(Iif(nNextMes==13,Year(dVencRea)+1,Year(dVencRea))),2))
                    Endif
                Endif
                //Acho o ultimo dia util do periodo desejado
                dNextday := DataValida(dNextday,.F.)
            Endif
        Else
            dNextday := fCRetCal(5, dVencRea)
        EndIf
    Endif

    // Ponto de entrada para que o cliente possa calcular
    // a data de vencimento
    If lF050MDVC
        dNextDay := ExecBlock("F050MDVC",.F.,.F.,{dNextDay,cImposto,dEmissao,dEmis1,dVencRea,cRetencao})
    EndIf

    If AliasInDic("FJQ")
        dDtFin790	:= dEmissao
        If cImposto $ "PIS#COFINS#CSLL" .and. lVencPcc
            dDtFin790 := dVencRea
        ElseIf cImposto $ "IRRF" .and. lVencIrrf
            dDtFin790 := dVencRea
        ElseIf cImposto $ "INSS" .and. lVenctoIN
            dDtFin790 := dVencRea
        EndIf
        aAreaSE2	:= GetArea()
        dbSelectArea("FJQ")
        dbSetOrder(1)
        If lVencCRet .and. FJQ->(FieldPos( "FJQ_CODRET" ))>0
            dbSelectArea("FJQ")
            dbSetOrder(1)
            If Iif(Empty(cRetencao), .F., FJQ->(dbSeek(xFilial("FJQ")+cRetencao)))
                nTipo	:= Val(FJQ->FJQ_PERIOD)
                dNextDay	:= fCRetCal(nTipo,dDtFin790)
            EndIf

        EndIF
        RestArea(aAreaSE2)
    EndIf

Return dNextDay

//-------------------------------------------------------
/*/{Protheus.doc} F050BxImp
Verifica se nenhum dos titulos de impostos relacionados a
um titulo em alteração, foram baixados

@author Mauricio Pequim Jr.
@since 23/01/04
@version P12
/*/
//-------------------------------------------------------
Function F050BxImp()

    Local lRet 			As Logical 
    Local aParc 		As Array 
    Local cTaxa 		As Character 
    Local aTipo 		As Array
    Local cChave        As Character 
    Local cTitPai 		As Character
    Local nX 			As Numeric
    Local nValImpos 	As Numeric
    Local aAreaSE2 		As Array
    Local aArea 		As Array
    //Controla o Pis Cofins e Csll na baixa
    Local lPCCBaixa 	As Logical
    Local cNatPcc 		As Character 
    Local lIRPFBaixa    As Logical 
    Local cNatIrf       As Character 
    Local lIRRFBaixa    As Logical 
    Local lVenctoINS  	As Logical 
    Local lVenctoIRF    As Logical 
    Local lVenctoISS    As Logical 
    Local lCalcIssBx    As Logical

    Default __lIsIssBx := FindFunction("IsIssBx")

    lRet 		  := .F.
    aParc 		  := {SE2->E2_PARCIR,SE2->E2_PARCINS,SE2->E2_PARCISS,SE2->E2_PARCSES,SE2->E2_PARCPIS,SE2->E2_PARCCOF,SE2->E2_PARCSLL}
    cTaxa 		  := IIF(SE2->E2_TIPO $ MVPAGANT+"/"+MV_CPNEG,MVTXA,MVTAXA)
    aTipo 		  := {cTaxa,IF(SE2->E2_TIPO $ MVPAGANT,"INA",MVINSS),MVISS,"SES",cTaxa,cTaxa,cTaxa}
    cChave 		  := SE2->(E2_PREFIXO+E2_NUM)
    cTitPai 	  := SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)
    nX 			  := 1
    nValImpos 	  := SE2->(E2_IRRF+E2_INSS+E2_ISS+E2_SEST+E2_PIS+E2_COFINS+E2_CSLL)
    aAreaSE2 	  := SE2->(GetArea())
    aArea 		  := GetArea()
    //Controla o Pis Cofins e Csll na baixa
    lPCCBaixa 	  := SuperGetMv("MV_BX10925",.T.,"2") == "1"
    cNatPcc 	  := GetMv("MV_PISNAT",.F.,"PIS") + "|" + GetMv("MV_CSLL",.F.,"CSL") + "|" + GetMv("MV_COFINS",.F.,"COF")
    lIRPFBaixa    := IIf( cPaisLoc == "BRA", SA2->A2_CALCIRF == "2", .F.)
    cNatIrf       := &(GetMv("MV_IRF",.F.,"'IRF'"))
    lIRRFBaixa    := IIf( cPaisLoc == "BRA", SA2->A2_CALCIRF == "2", .F.)
    lVenctoINS    := SuperGetMv("MV_VENCINS",.T.,"1") == "2"  //1 = Emissao   2= Vencimento Real
    lVenctoIRF    := SuperGetMv("MV_VENCIRF", .F.,"V") == "V" //E = Emissao   V= Vencimento Real      C = Contabilização (EMIS1) 
    lVenctoISS    := SuperGetMv("MV_VENCISS", .F.,"V") == "V" //E = Emissao   V= Vencimento Real      C = Contabilização (EMIS1) 
    lCalcIssBx    := IIF(__lIsIssBx, IsIssBx("P"), SuperGetMv("MV_MRETISS",.F.,"1") == "2" )

    If nValImpos > 0
        dbSelectArea("SE2")
        dbSetOrder(17) // E2_FILIAL + E2_TITPAI
        If MsSeek(xFilial("SE2")+cTitPai)
            While SE2->(!EOF()) .And. Alltrim(SE2->E2_TITPAI) == Alltrim(cTitPai) .And. !lRet
                If !lPCCBaixa .and. Alltrim(SE2->E2_NATUREZ) $ cNatPcc .AND. (!IsInCallStack("FA050Delet") .OR. cValToChar(nX) $ "5|6|7")

                    If ( SE2->E2_SALDO == 0)
                        //-- Tratamento para o parêmetro MV_BX10925 == 2
                        If SE2->E2_TIPO $ aTipo[1] .And. (SE2->E2_PARCELA == aParc[1] .Or. SE2->E2_PARCELA == aParc[5] .Or.;
                        SE2->E2_PARCELA == aParc[6] .Or. SE2->E2_PARCELA == aParc[7] )

                            //-- Se existir títulos de PCC baixados não será possível alterar!
                            If nX == 1 .and. (!lIRRFBaixa .or. !lVenctoIRF )
                                DbSkip()
                            Else
                                lRet 	:= .T.
                            EndiF

                        Else
                            DbSkip()
                        EndIf
                    Else
                        DbSkip()
                    EndIf
                ElseIf !lIRPFBaixa .and. AllTrim(SE2->E2_NATUREZ) == cNatIrf
                    If SE2->E2_SALDO == 0 .and. lVenctoIRF
                        lRet := .T.
                    Else
                        dbSkip()
                    Endif
                ElseIf AllTrim(SE2->E2_TIPO) $ MVINSS
                    If SE2->E2_SALDO == 0 .and. lVenctoINS  //Vencimento do INSS pela Vencimento Real do título
                        lRet := .T.
                    Else
                        dbSkip()
                    Endif
                ElseIf !lCalcIssBx .And. AllTrim(SE2->E2_TIPO) $ MVISS 
                    If SE2->E2_SALDO == 0 .and. lVenctoISS  //Vencimento do ISS pela Vencimento Real do título
                        lRet := .T.
                    Else
                        dbSkip()
                    Endif
                Else
                    If SE2->E2_SALDO == 0 
                        lRet := .T.
                    Else
                        dbSkip()
                    EndIf
                EndIf

                nX++
            Enddo
        Endif
    Endif
    RestArea(aAreaSE2)
    RestArea(aArea)
    FwFreeArray(aParc)
    FwFreeArray(aTipo)

Return lRet

//-------------------------------------------------------
/*/{Protheus.doc} VerInssAcm
Verifica o valor RETIDO de INSS de um fornecedor
num determinado periodo.

@author Mauricio PEquim Jr.
@since 11/08/2004
@version 12
@return nValor, Valor acumulado do INSS
@param cFornece, characters, Código do fornecedor
@param cLoja, characters, Loja do fornecedor
@param dEmissao, date,  Data de emissão do título financeiro
@param dVencRea, date,  Data de vencimento real do título financeiro
@param lNFE, logical, Indica se foi chamado no cálculo pela nota fiscal
@type function
/*/
//-------------------------------------------------------
Function VerInssAcm( cFornece, cLoja, cNomeFor, dEmissao, dVencRea, lNFE)

    Local nValor	 := 0
    Local aArea		 := GetArea()
    Local lFilInss	 := SuperGetMV("MV_FILINSS",.T.,.F.)
    Local aFilINSS	 := {}
    Local nX		 := 0
    Local nRegSM0	 := 0
    Local cEmpAtu	 := ""
    Local cCnpj		 := ""
    Local cTablTemp	 := ""
    Local LFINA050	 := .F.
    Local nInssPLS	 := 0

    DEFAULT cFornece := ""
    DEFAULT cLoja	 := ""
    DEFAULT cNomeFor := ""
    DEFAULT dEmissao := CTOD("//")
    DEFAULT dVencRea := CTOD("//")
    DEFAULT lNFE	 := .F.

    //Valida se tem a previa de INSS
    If __lInsPrev == NIL
        __lInsPrev := AliasInDic("FJW") .And. FindFunction("F027PRINSS")
    Endif

    //Valida se tem o campo do PLS para obtenção do INSS acumulado no formato antigo
    If __lPlOpeLt == NIL
        __lPlOpeLt := __lPlOpeLt := SE2->(FieldPos("E2_PLOPELT")) > 0
    Endif

    //Verifico todas as filiais apenas quando SA2 compartilhado
    If ExistBlock("F50TFINS")
        aFilINSS := ExecBlock( "F50TFINS", .F., .F. )
    Else
        If lFilInss
            nRegSM0 := SM0->( RECNO() )
            cEmpAtu := SM0->M0_CODIGO
            cCnpj := Substr( SM0->M0_CGC, 1, 8 )

            If SM0->( msSeek( cEmpAtu ) )
	            While SM0->( !EOF() ) .And. SM0->M0_CODIGO == cEmpAtu
	                If Substr( SM0->M0_CGC, 1, 8 ) == cCnpj
	                    AAdd( aFilINSS, SM0->M0_CODFIL )
	                EndIf
	                SM0->( dbSkip() )
	            EndDo
	        EndIf

            SM0->( dbGoto(nRegSM0) )
        Else
            aFilINSS := { FWxFilial("SE2") }
        Endif
    Endif

    If !Empty(cFornece) .And. !Empty(cLoja)

        cQuery := "SELECT SUM(E2_INSS) NVALINSS FROM " + RetSQLname("SE2")
        cQuery += " WHERE "
        cQuery += "( "
        For nX:= 1 to Len(aFilINSS)
            cQuery += "E2_FILIAL = '" + aFilINSS[nX] + "' OR "
        Next nX
        cQuery := Left( cQuery, Len( cQuery ) - 4 )
        cQuery += ") AND "

        cQuery += "E2_FORNECE = '" + cFornece + "' AND "
        cQuery += "E2_LOJA = '" + cLoja + "' AND "
        cQuery += "E2_INSS > 0 AND "

        If Type("M->E2_NUM") == "C" .AND. Type("M->E2_PREFIXO") == "C" .AND. ;
            Type("M->E2_PARCELA") == "C" .AND. Type("M->E2_TIPO") == "C"

            If !lFilInss
                cQuery += "NOT(E2_NUM = '"+ M->E2_NUM +"' AND "
                cQuery += "E2_PREFIXO = '"+ M->E2_PREFIXO +"' AND "
                cQuery += "E2_PARCELA = '"+ M->E2_PARCELA +"' AND "
                cQuery += "E2_TIPO = '"+ M->E2_TIPO +"') AND "
            EndIf
        EndIf

        cQuery += FiltDtINSS( dEmissao, dVencRea )

        cQuery += "D_E_L_E_T_ = ' ' "
        cQuery := ChangeQuery(cQuery)

        cTablTemp := GetNextAlias()
        dbUseArea( .T. , "TOPCONN" , TCGenQry(,,cQuery), cTablTemp , .F. , .T. )

        If ( cTablTemp )->( ! EOF() )
        	TcSetField( cTablTemp , "NVALINSS" , "N" , 17 , 2 )
        	nValor := ( cTablTemp )->NVALINSS
		Endif

        ( cTablTemp )->( dbCloseArea() )

        //Retorna o saldo de INSS Previas de INSS / INSS outras empresas
        If __lInsPrev
            LFINA050 := FUNNAME() == "FINA050"

            If lNFE
                nInssPLS := F027PRINSS( SA2->A2_COD, SA2->A2_LOJA, dEmissao, dVencrea )
            ElseIf !Empty(M->E2_TIPO) .And. (!M->E2_TIPO $ MVPAGANT) .Or. (LFINA050 .And. M->E2_TIPO $ MVPAGANT )
                nInssPLS := F027PRINSS( SA2->A2_COD, SA2->A2_LOJA, M->E2_EMISSAO, M->E2_VENCREA )
            EndIf
        EndIf

        nValor += nInssPLS
    Endif

    RestArea( aArea )
    FwFreeArray( aArea )
    FwFreeArray( aFilINSS )

Return nValor

//-------------------------------------------------------
/*/{Protheus.doc} VerInssCalc
Verifica o valor CALCULADO de INSS de um fornecedor num determinado periodo.

@author Adrianne Furtado
@since 11/08/2004
@version 12
@return nValor, Valor acumulado do INSS
@param cFornece, characters, Código do fornecedor
@param cLoja, characters, Loja do fornecedor
@param cNomeFor, characters, Nome reduzido do fornecedor
@param dEmissao, date,  Data de emissão do título financeiro
@param dVencRea, date,  Data de vencimento real do título financeiro
@param aRecINSS, array, Recnos dos títulos com INSS, conforme os parâmetros de filtro - Passado por referência
@param nInsRest, numeric, Valor do INSS calculado - Passado por referência
@type function
/*/
//------------------------------------------------------------------------------------------
Function VerInssCalc(cFornece,cLoja,cNomeFor,dEmissao,dVencRea,aRecINSS,nInsRest,lRetPer)

    Local nValor 	 := 0
    Local aArea 	 := {}
    Local aAreaSA2	 := {}
    Local aFilINSS 	 := {}
    Local nX		 := 0
    Local cAliasTmp  := ""
    Local cPonteiro  := ""

    Default aRecINSS := {}
    Default nInsRest := 0
    Default lRetPer	:= .F.

    If !Empty(cFornece) .And. !Empty(cLoja) .And. !Empty(dEmissao)
    	aArea := GetArea()
    	cAliasTmp := GetNextAlias()
    	cPonteiro := Iif( Type("M->E2_NUM") == "C", "M->", "SE2->" )

    	//Verifico todas as filiais apenas quando SA2 compartilhado
	    If ExistBlock("F50TFINS")
	        aFilINSS := ExecBlock( "F50TFINS", .F., .F. )
	    Else
	        aFilINSS := { xFilial("SE2") }
	    Endif

        //Valida se o fornecedor calcula acumulado - PF
        aAreaSA2 := SA2->( GetArea() )
        SA2->( dbSetOrder(1) ) //A2_FILIAL+A2_COD+A2_LOJA
        If SA2->( msSeek( FWxFilial("SA2") + cFornece + cLoja ) )
            cTpForn := SA2->A2_TIPO
        EndIf
        RestArea( aAreaSA2 )
        FwFreeArray( aAreaSA2 )

        cQuery := "SELECT SUM(E2_VRETINS) NVALINSS FROM " + RetSQLname("SE2")
        cQuery += " WHERE "
        cQuery += "( "
        For nX:= 1 to Len(aFilINSS)
            cQuery += "E2_FILIAL = '" + aFilINSS[nX] + "' OR "
        Next nX
        cQuery := Left( cQuery, Len( cQuery ) - 4 )
        cQuery += ") AND "
        cQuery += "E2_FORNECE = '"+ cFornece +"' AND "
        cQuery += "E2_LOJA = '" + cLoja + "' AND "
        cQuery += "E2_VRETINS > 0 AND "
        cQuery += "E2_PRETINS = '1' AND " //E2_PRETINS = '1' -> Pendente de retenção.
        cQuery += "NOT(E2_NUM = '" + &(cPonteiro + "E2_NUM") + "' AND "
        cQuery += "E2_PREFIXO = '" + &(cPonteiro + "E2_PREFIXO") + "' AND "
        cQuery += "E2_PARCELA = '" + &(cPonteiro + "E2_PARCELA") + "' AND "
        cQuery += "E2_TIPO = '" + &(cPonteiro + "E2_TIPO") + "') AND "
        If cTpForn $ "F;J"
            cQuery += FiltDtINSS( dEmissao, dVencRea )
        EndIf
        cQuery += "D_E_L_E_T_ = ' ' "
        cQuery := ChangeQuery(cQuery)

        dbUseArea(.T., "TOPCONN", TCGenQry(,,cQuery), cAliasTmp, .F., .T.)
        If ( cAliasTmp )->( ! EOF() )
        	TcSetField( cAliasTmp, "NVALINSS", "N", 17, 2 )
        	nValor := ( cAliasTmp )->NVALINSS
        EndIf
        ( cAliasTmp )->( dbCloseArea() )

        If nValor > 0
            cQuery := "SELECT R_E_C_N_O_ RECNO FROM " + RetSQLname("SE2")
            cQuery += " WHERE "
            cQuery += "( "
            For nX:= 1 to Len(aFilINSS)
                cQuery += "E2_FILIAL = '"+ aFilINSS[nX] + "' OR "
            Next nX
            cQuery := Left( cQuery, Len( cQuery ) - 4 )
            cQuery += ") AND "
            cQuery += "E2_FORNECE = '"+ cFornece +"' AND "
            cQuery += "E2_LOJA = '"+ cLoja +"' AND "
            cQuery += "E2_VRETINS > 0 AND "
            cQuery += "NOT(E2_NUM = '" + &(cPonteiro + "E2_NUM") + "' AND "
            cQuery += "E2_PREFIXO = '" + &(cPonteiro + "E2_PREFIXO") + "') AND "
            If FwIsInCallStack("FGRVINSS")
                cQuery += "E2_PRETINS = '1' AND "	//E2_PRETINS = '1' -> Pendente de retenção.
            Else
                cQuery += FiltDtINSS( dEmissao, dVencRea )
            EndIf
            cQuery += "D_E_L_E_T_ = ' ' "
            cQuery := ChangeQuery(cQuery)

            dbUseArea(.T., "TOPCONN", TCGenQry(,,cQuery), cAliasTmp, .F., .T.)
            While ( cAliasTmp )->( !EOF() )
                AAdd( aRecINSS, ( cAliasTmp )->RECNO )
                ( cAliasTmp )->( dbSkip() )
            EndDo
            ( cAliasTmp )->( dbCloseArea() )
        Endif

        //Verifico os valores retidos e que não foram calculados
        cQuery := "SELECT SUM(E2_INSS) NVLINSCAL, SUM(E2_VRETINS) NVLINSRET FROM " + RetSQLname("SE2")
        cQuery += " WHERE "
        cQuery += "E2_FILIAL = '"+ xFilial("SE2") + "' AND "
        cQuery += "E2_FORNECE = '"+ cFornece +"' AND "
        cQuery += "E2_LOJA = '"+ cLoja +"' AND "
        cQuery += "E2_VRETINS > 0 AND "
        cQuery += "(E2_PRETINS = '2' OR E2_PRETINS ='') AND "
        cQuery += "NOT(E2_NUM = '" + &(cPonteiro + "E2_NUM") + "' AND "
        cQuery += "E2_PREFIXO = '" + &(cPonteiro + "E2_PREFIXO") + "') AND "
        cQuery += FiltDtINSS( dEmissao, dVencRea )
        cQuery += "D_E_L_E_T_ = ' ' "
        cQuery := ChangeQuery(cQuery)

        dbUseArea(.T., "TOPCONN", TCGenQry(,,cQuery), cAliasTmp, .F., .T.)
        If ( cAliasTmp )->( ! EOF() )
        	nInsRest := ( cAliasTmp )->NVLINSRET - ( cAliasTmp )->NVLINSCAL
        	If ( cAliasTmp )->NVLINSRET > 0
				lRetPer	:= .T.
			Else
				lRetPer	:= .F.
			EndIf
        EndIf
        ( cAliasTmp )->( dbCloseArea() )

        RestArea( aArea )
        FwFreeArray( aArea )
        FwFreeArray( aFilINSS )
    Endif

Return nValor

//-------------------------------------------------------------------
/*/{Protheus.doc} FCalcInsPF
Calcula o INSS de Pessoa Fisica respeitando os limites de retencao.

@author Mauricio Pequim Jr
@since 11/08/2004
@version 12
@return nValInss, Valor do INSS calculado
@param nValBase, numeric, Valor de base de cálculo do INSS
@param nCalcInss, numeric, Valor já calculado do INSS - Passado por referência
@param nINSSTot, numeric, Total de INSS calculado (acumulado) - Passado por referência
@param lRegraComp, logical, Indica se irá considerar as regras de complemento de imposto
@param nDedValor, numeric, Valor alterado do INSS, conforme regra de complemento de imposto - Passado por referência
@param lNFE, logical, Indica se foi chamado no cálculo pela nota fiscal
@param dEmissao, date, Data de emissão do título financeiro gerado pela nota
@param dVencrea, date, Data de vencimento real do título financeiro gerado pela nota
@type function
/*/
//-------------------------------------------------------------------
Function FCalcInsPF(nValBase, nCalcInss, nINSSTot, lRegraComp, nDedValor, lNFE, dEmissao, dVencrea, lFunRural)

    Local nLimInss     := GetMv("MV_LIMINSS", .F., 0)
    Local nInssAcum    := 0
    Local nInssCalc    := 0
    Local nValMaxIns   := 0
    Local lRoundIns	   := .F.
    Local nVlMinINSS   := SuperGetMv("MV_MININSS", .F., 0) //Esse parametro deve estar preenchido com o valor mínimo para recolhimento de INSS o conteúdo padrão "0" (zero) foi utilizado para manter o legado.
    Local lPaBruto	   := .F.
    Local lPrImPA      := .F.
    Local nValInss     := 0
    Local nInsRest     := 0
    Local lOk	       := .T.
    Local nDedBase	   := 0
    
    Default nDedValor  := 0
    Default nCalcInss  := 0
    Default nINSSTot   := 0
    Default lRegraComp := .T.
    Default lNFE       := .F.
    Default dEmissao   := M->E2_EMISSAO
    Default dVencrea   := M->E2_VENCREA
    Default lFunRural  := .F.

    INCLUI := Iif( Type("INCLUI") == "U", .F., INCLUI )
    ALTERA := Iif( Type("ALTERA") == "U", .T., ALTERA )

    If lFunRural
        nLimInss := 0
    EndIf

    //valor do INSS para ESSE título.
    If !Empty(nCalcInss) .And. lNFE
        nValInss := nCalcInss
    Else
        If __lLocBRA .And. lRegraComp
        	nDedBase := Fa986regra("SE2", "INSS", "1") //Busca regra de complemento do INSS, para alteração da base de cálculo (FKG_APLICA = "1")
    		nDedValor := Fa986regra("SE2", "INSS", "2") //Busca regra de complemento do INSS, para alteração do valor calculado (FKG_APLICA = "2")
    	EndIf

    	nValBase := nValBase + nDedBase

	    If nValBase < 0
	        nValBase := 0
	    EndIf

    	lRoundIns := GetNewPar("MV_RNDINS",.F.)
    	If lRoundIns
	        nValInss := Round( ( nValBase * (SED->ED_PERCINS / 100) ), 2 )
	    Else
	        nValInss := NoRound( ( nValBase * (SED->ED_PERCINS / 100) ), 2 )
	    EndIf

	    nValInss := nValInss + nDedValor

	    If nValInss < 0
	        nValInss := 0
	    EndIf

        nCalcInss := nValInss

        nValInss := FRetOTits(nValInss) //na alteracao, verifica se tem impostos de outros titulos
    EndIf

    //Retornar o saldo de INSS acumulado do fornecedor, do mês de emissao ou vencimento do título originador
    nInssAcum := VerInssAcm(SA2->A2_COD, SA2->A2_LOJA,, dEmissao, dVencrea, lNFE)

    nValMaxIns := (nLimInss - nInssAcum) //Saldo do que pode ser retido no mes

    //Retornar o saldo de INSS calculado para o fornecedor, do mes de emissao ou vencimento do titulo originador
    nINSSCalc := VerInssCalc(SA2->A2_COD, SA2->A2_LOJA, SA2->A2_NREDUZ, dEmissao, dVencrea,, @nInsRest)

    nINSSTot := (nInssCalc + nValINSS) + Iif(nInsRest > 0 .And. !ALTERA, nInsRest, 0)

    If !lNFE
        lPaBruto := GetNewPar("MV_PABRUTO", "2") == "1" //Indica se o PA terá o valor dos impostos descontados do seu valor
        lPrImPA := !lPaBruto .And. SuperGetMv("MV_PAPRIME", .T., "2") == "1"

        lOk := !( M->E2_TIPO == MVPAGANT .And. lPrImPA )
    Endif

    If nVlMinINSS > 0 .And. lOk //Se o valor devido de INSS do título for menor que o valor mínimo. Ele será zerado.
        If nINSSTot+nInssAcum < nVlMinINSS
            nValINSS := 0
            nINSSTot := 0
            If nLimInss <> 0 .And. nValMaxIns == 0
                nCalcInss := 0
            EndIf
        EndIf
    EndIf

    If nLimInss > 0 .And. nValINSS <> 0 .And. nINSSTot <> 0
        //Retornar o saldo de INSS acumulado do fornecedor, do mes de emissao ou vencimento do titulo originador
        If nValMaxIns <= 0
            nValInss := 0
            nINSSTot := 0
            nCalcInss := 0
        ElseIf nValMaxIns < nVlMinINSS
            //Se o valor retido (retencao por nao atingir o minimo de retencao e/ou por haver ultrapassado o maximo) for um saldo,
            //verificar se o seu valor nao eh igual ou maior ao do saldo calculado, para evitar duplicidade de retencao de INSS
            If nINSSCalc < nVlMinINSS
                If nINSSCalc >= nValMaxIns
                    nCalcInss := 0
                Else
                    nCalcInss := nValMaxIns - nINSSCalc
                Endif
            Else
                nCalcInss := nValMaxIns
            Endif
            nValInss := 0
            nINSSTot := 0
        Else
            //Atribuir ao INSS calculado do titulo, as retencoes pendentes
            nValInss += nINSSCalc
            If nValMaxIns < nValInss
                nValInss  := nCalcInss :=  nValMaxIns
            EndIf
            If nValMaxIns < nINSSTot
                nINSSTot :=  IIf(nValMaxIns > nValInss, nValInss, nValMaxIns)
            EndIf
        Endif
    Endif

Return nValInss

//-------------------------------------------------------------------
/*/{Protheus.doc} FCalcInsPJ
Calcula o INSS de Pessoa Jurídica respeitando os limites de retencao.

@author Mauricio Pequim Jr
@since 11/08/2004
@version 12
@return nValInss, Valor do INSS calculado
@param nValBase, numeric, Valor de base de cálculo do INSS
@param nCalcInss, numeric, Valor já calculado do INSS - Passado por referência
@param nINSSTot, numeric, Total de INSS calculado (acumulado) - Passado por referência
@param lRegraComp, logical, Indica se irá considerar as regras de complemento de imposto
@param nDedValor, numeric, Valor alterado do INSS, conforme regra de complemento de imposto - Passado por referência
@param lNFE, logical, Indica se foi chamado no cálculo pela nota fiscal
@param dEmissao, date, Data de emissão do título financeiro gerado pela nota
@param dVencrea, date, Data de vencimento real do título financeiro gerado pela nota
@type function

@obs Orientação do artigo 120 da IN 971/2009

     Fica dispensada a retenção quando o valor correspondente a 11% do valor dos serviços
     prestados for inferior ao limite mínimo estabelecido para recolhimento.

     Este tratamento deve considerar individualmente cada documento.
/*/
//-------------------------------------------------------------------
Function FCalcInsPJ(nValBase, nCalcInss, nINSSTot, lRegraComp, nDedValor, lNFE, dEmissao, dVencrea)

    Local nInssCalc    := 0
    Local lRoundIns	   := .F.
    Local nVlMinINSS   := 0
    Local lAcmPJ	   := .F.
    Local lPaBruto	   := .F.
    Local lPrImPA	   := .F.
    Local nInsRest	   := 0
    Local lOk		   := .F.
    Local nDedBase	   := 0
    Local lFina377	   := FUNNAME() == "FINA377"
    Local lRetPer		:= .F.
    Local cPonteiro    := ""
    Local nInssAcum	   := 0

    Default nCalcInss  := 0
    Default nINSSTot   := 0
    Default lRegraComp := .T.
    Default nDedValor  := 0
    Default lNFE	   := .F.
    Default dEmissao   := M->E2_EMISSAO
    Default dVencrea   := M->E2_VENCREA

    INCLUI := Iif(Type("INCLUI") == "U", .F., INCLUI)
    ALTERA := Iif(Type("ALTERA") == "U", .T., ALTERA)
    
    //valor do INSS para ESSE título.
    If !Empty(nCalcInss) .And. lNFE
        nValInss := nCalcInss
    Else
	    If __lLocBRA .And. lRegraComp
	    	nDedBase := Fa986regra("SE2", "INSS", "1") //Busca regra de complemento do INSS, para alteração da base de cálculo (FKG_APLICA = "1")
	    	nDedValor := Fa986regra("SE2", "INSS", "2") //Busca regra de complemento do INSS, para alteração do valor calculado (FKG_APLICA = "2")
	    EndIf

    	nValBase := nValBase + nDedBase

	    If nValBase < 0
	        nValBase := 0
	    EndIf

    	lRoundIns := GetNewPar("MV_RNDINS",.F.)
        If lRoundIns
	        nValInss := Round( ( nValBase * (SED->ED_PERCINS / 100) ), 2 )
	    Else
	        nValInss := NoRound( ( nValBase * (SED->ED_PERCINS / 100) ), 2 )
	    EndIf
	    nValInss := nValInss + nDedValor

	    If nValInss < 0
	        nValInss := 0
	    EndIf

        nCalcInss := nValInss
    EndIf

    If !lFina377
    	lPaBruto := GetNewPar("MV_PABRUTO", "2") == "1" //Indica se o PA terá o valor dos impostos descontados do seu valor
    	lPrImPA := !lPaBruto .And. SuperGetMv("MV_PAPRIME", .T., "2") == "1" //Indica se irá provisionar os impostos de INSS e ISS na inclusão da PA, deduzindo-os do valor de adiantamento.

        cPonteiro := Iif( FWIsInCallStack("FINA050"), "M->", "SE2->" )
        lOk := !( &(cPonteiro + "E2_TIPO") == MVPAGANT .And. lPrImPA )
    Endif

    If lOk
        nVlMinINSS := SuperGetMv("MV_VLRETIN", .F., 0) //Esse parametro deve estar preenchido com o valor mínimo para recolhimento de INSS o conteúdo padrão "0" (zero) foi utilizado para manter o legado.

        If nVlMinINSS > 0
            lAcmPJ := SuperGetMv("MV_INSACPJ", .T., "2") == "1" //1 = Acumula  2= Não acumula

            If lAcmPJ
             	//Retornar o saldo de INSS acumulado do fornecedor, do mês de emissao ou vencimento do título originador
                nInssAcum := VerInssAcm(SA2->A2_COD, SA2->A2_LOJA,, dEmissao, dVencrea, lNFE)
                nInssCalc := VerInssCalc(SA2->A2_COD, SA2->A2_LOJA, SA2->A2_NREDUZ, dEmissao, dVencrea,, @nInsRest,@lRetPer)
                nInsRest := Iif( nInsRest > 0, nInsRest, 0 )
            EndIf

            nINSSTot := nInssCalc + nValINSS + nInsRest

            If ( !lRetPer .And. nINSSTot + nInssAcum < nVlMinINSS) .And. ( Iif( Existblock("FinVldIns"), Execblock("FinVldIns", .F., .F., {} ), .T. ) )
                nValINSS := 0
                nINSSTot := 0

                If !lAcmPJ
                    nCalcInss := 0
                EndIf
            EndIf
        Else
            nINSSTot := nValINSS
        EndIf
    EndIf

Return nValInss

//-------------------------------------------------------------------
/*/{Protheus.doc} FVerMinImp
Verifica o valor minimo de retencao dos impostos PIS, COFINS e CSLL

@param nValor, numeric, Valor de referência usado no cálculo do PCC
@param lButMenu, logical, Indica se foi chamada da tela de modalidade
                 de retenção do PCC
@return Nil

@author Mauricio Pequim Jr
@since 02/02/04
@type function
/*/
//-------------------------------------------------------------------
Function FVerMinImp(nValor, lButMenu,lIrfRetAnt)

    Local nVlMinImp := 0
    Local nCond := 0
    Local nRecAtuSE2 := SE2->( RECNO() )
    Local nValNdf := 0
    Local nPisNdf := 0
    Local nCofNdf := 0
    Local nCslNdf := 0
    Local nValorTit := 0
    Local nTotImp := 0
    Local nX := 0
    Local aRecSE2 := {}
    Local cNccRet := ""
    Local nRetOriPIS := 0
    Local nRetOriCOF := 0
    Local nRetOriCSL := 0
    Local lBaseSE2 := .F.
    Local lPCCBaixa  := SuperGetMv("MV_BX10925",.T.,"2") == "1" //Controla o Pis Cofins e Csll na baixa
    Local lEmpPub    := IsEmpPub()
    Local dDtTotM    := CtoD("  /  /  ")
    Local nVencto 	:=  SuperGetMv("MV_VCPCCP",.T.,1) 

    Default nValor := M->E2_VALOR
    Default lButMenu := .F.
    Default lIrfRetAnt  := .F.

    //Verificacao para outros modulos
    cModRetPIS	:= Iif( Type("cModRetPis") != "C", GetNewPar("MV_RT10925", "1"), cModRetPIS )
    aDadosRet   := Iif( Type("aDadosRet") != "A", Array(5), aDadosRet )
    cOldNaturez	:= Iif( Type("cOldNaturez") != "C", "", cOldNaturez )
    nRecnoNdf 	:= Iif( Type("nRecnoNdf") != "N", 0, nRecnoNdf )
    nDifPcc		:= Iif( Type("nDifPcc") != "N", 0, nDifPcc )
    aDadosImp   := Iif( Type("aDadosImp") != "A", Array(3), aDadosImp )
    
    If Type("lAltera") != "L"
        lAltera := .F.
    EndIf

    If lAltera
        lAlterNat := .T.
        aRecSE2 := FImpExcTit("SE2", SE2->E2_PREFIXO, SE2->E2_NUM, SE2->E2_PARCELA, SE2->E2_TIPO, SE2->E2_FORNECE, SE2->E2_LOJA)
        For nX := 1 to Len(aRecSE2)
            SE2->( MSGoto( aRecSE2[nX] ) )
            If SE2->E2_TIPO $ MV_CPNEG
                If STR(SE2->E2_SALDO, 17, 2) != STR(SE2->E2_VALOR, 17, 2)
                    nValNdf := 0
                    nRecnoNdf := 0
                Else
                    nValNdf := SE2->E2_SALDO
                    nRecnoNdf := aRecSE2[nX]
                    Exit
                Endif
            Endif
        Next nX
        SE2->( dbGoTo(nRecAtuSE2) )
        FwFreeArray(aRecSE2)
    Endif

    nPisOri := IIf(Type("nPisOri") != "N" , 0, nPisOri)
    nCofOri := IIf(Type("nCofOri") != "N" , 0, nCofOri)
    nCslOri := IIf(Type("nCslOri") != "N" , 0, nCslOri)

    If __lLocBRA .And. (M->E2_PIS + M->E2_COFINS + M->E2_CSLL > 0)
        If cModRetPis == "3"  //Se nao retem PCC
            nVlRetPis := M->E2_PIS
            nVlRetCof := M->E2_COFINS
            nVlRetCsl := M->E2_CSLL
            M->E2_PIS := 0
            M->E2_COFINS := 0
            M->E2_CSLL := 0
        Else
            nVlMinImp := GetNewPar("MV_VL10925", 5000)

            If nVencto == 2 .OR. lPCCBaixa
				 dDtTotM := M->E2_VENCREA
			ElseIf nVencto == 1 .OR. EMPTY(nVencto)
                dDtTotM := M->E2_EMISSAO
			ElseIf nVencto == 3
                dDtTotM := M->E2_EMIS1 
			Endif
            If !lEmpPub .Or. (lEmpPub .And. lPccBaixa ) .Or. (!lPccBaixa .And. lEmpPub .And. !lIrfRetAnt) 
                aDadosRet := F050TotMes(dDtTotM)
            EndIf    

            // Guarda os valores originais
            nRetOriPIS := M->E2_PIS
            nRetOriCOF := M->E2_COFINS
            nRetOriCSL := M->E2_CSLL

            If !lAltera
                //1-Cria NCC/NDF referente a diferenca de impostos entre emitidos (SE2) e retidos (SE5)
                //2-Nao Cria NCC/NDF, ou seja, controla a diferenca num proximo titulo
    			//3-Nao Controla
                cNccRet := SuperGetMv("MV_NCCRET",.F.,"1")

                If M->E2_PIS > 0 .And. aDadosImp[1] <> aDadosRet[2]
                    If cNCCRet == "2"
                        M->E2_PIS += aDadosImp[1]
                    EndIf
                EndIf
                If M->E2_COFINS > 0  .And. aDadosImp[2] <> aDadosRet[3]
                    If cNCCRet == "2"
                        M->E2_COFINS += aDadosImp[2]
                    EndIf
                EndIf
                If M->E2_CSLL > 0  .And. aDadosImp[3] <> aDadosRet[4]
                    If cNCCRet == "2"
                        M->E2_CSLL += aDadosImp[3]
                    EndIf
                EndIf
            EndIf

            //Cálculo do Sistema
            IF cModRetPis == "1"
                If lAltera .And. nValor > nVlMinImp
                    nCond := aDadosRet[1] + nValor

                    //Tratamento para base de impostos diferenciado
                    lBaseSE2 := __lLocBRA .And. SuperGetMv("MV_BS10925",.T.,"1") == "1"
                    If lBaseSE2 .And. SE2->E2_BASEPIS > 0
                        nCond -= SE2->E2_BASEPIS
                    Else
                        nCond -= ( SE2->(E2_VALOR + E2_IRRF + E2_INSS + E2_ISS + E2_VRETPIS + E2_VRETCOF + E2_VRETCSL + E2_SEST) )
                    Endif
                Else
                    nCond := aDadosRet[1] + nValor
                Endif
                If (nCond <= nVlMinImp .And. nValor  <= nVlMinImp) .Or. lButMenu
                    nVlRetPis := M->E2_PIS
                    nVlRetCof := M->E2_COFINS
                    nVlRetCsl := M->E2_CSLL
                    M->E2_PIS := 0
                    M->E2_COFINS:= 0
                    M->E2_CSLL 	:= 0
                Endif
            Endif

            If M->E2_PIS + M->E2_COFINS + M->E2_CSLL > 0
                nVlRetPis    := M->E2_PIS
                nVlRetCof	 := M->E2_COFINS
                nVlRetCsl	 := M->E2_CSLL
                M->E2_PIS	 := nVlRetPis + Iif( aDadosImp[1] <> aDadosRet[2] .And. lAltera, aDadosImp[1], aDadosRet[2] )
                M->E2_COFINS := nVlRetCof + Iif( aDadosImp[2] <> aDadosRet[3] .And. lAltera, aDadosImp[2], aDadosRet[3] )
                M->E2_CSLL	 := nVlRetCsl + Iif( aDadosImp[3] <> aDadosRet[4] .And. lAltera, aDadosImp[3], aDadosRet[4] )

                If lAltera
                    //Proporcionalizar o valor da NDF para os impostos
                    If nValNdf > 0
                        nValorTit := SE2->(E2_VALOR + E2_PIS + E2_COFINS + E2_CSLL + E2_IRRF + E2_INSS + E2_ISS + E2_SEST)
                        nTotImp := SE2->(E2_PIS + E2_COFINS + E2_CSLL)
                        nPisNdf := Round( (SE2->E2_PIS * nValNdf) / nTotImp, 2 )
                        nCofNdf := Round( (SE2->E2_COFINS * nValNdf) / nTotImp, 2 )
                        nCslNdf := nValNdf - (nPisNdf + nCofNdf)
                        M->E2_PIS += nPisNdf
                        M->E2_COFINS += nCofNdf
                        M->E2_CSLL += nCslNdf
                    Endif
                Endif
                f050VerVlr(nValor)
            Else
                //Natureza nao calculou Pis/Cofins/Csll
                AFill( aDadosRet, 0 )
            Endif

            //Restauro os valores originais
            nVlRetPis := nRetOriPIS
            nVlRetCof := nRetOriCOF
            nVlRetCsl := nRetOriCSL

            If lAltera .And. IIf( Type("nOldValor") == "N", nOldValor == M->E2_VALOR, .T. ) .And. !(!lPCCBaixa .And. lEmpPub )
                If SE2->E2_BASEPIS + aDadosRet[1] > nVlMinImp .And. Month(SE2->E2_VENCREA) <> Month(M->E2_VENCREA) .And. SE2->(E2_VRETPIS + E2_VRETCOF + E2_VRETCSL) > 0
                    M->E2_PIS := SE2->E2_VRETPIS
                    M->E2_COFINS := SE2->E2_VRETCOF
                    M->E2_CSLL := SE2->E2_VRETCSL
                ElseIf Month(SE2->E2_VENCREA) == Month(M->E2_VENCREA)
                    M->E2_PIS := SE2->E2_PIS
                    M->E2_COFINS := SE2->E2_COFINS
                    M->E2_CSLL := SE2->E2_CSLL
                ElseIf !( Month(SE2->E2_EMISSAO) == Month (M->E2_VENCREA) )
                    M->E2_PIS := 0
                    M->E2_COFINS := 0
                    M->E2_CSLL := 0
                EndIf
            EndIf
        Endif
    Else
        //Natureza nao calculou Pis/Cofins/Csll
        AFill( aDadosRet, 0 )
        AFill( aDadosImp, 0 )
        nVlRetPis := M->E2_PIS
        nVlRetCof := M->E2_COFINS
        nVlRetCsl := M->E2_CSLL
    Endif

Return

//-------------------------------------------------------------------
/*/{Protheus.doc} F050TotMes
Verifica o total de notas do Fornecedor que vencem no mesmo mes

@param dReferencia, date, Data de referência
@return aDadosRef, Vetor com os dados do PCC
@sample aDadosRef[1] = Total de retenção de PCC
		aDadosRef[2] = Total de retenção de PIS
		aDadosRef[3] = Total de retenção de COFINS
		aDadosRef[4] = Total de retenção de CSLL

@author Mauricio Pequim Jr
@since 23/08/2004
@type function
/*/
//-------------------------------------------------------------------
Function F050TotMes(dReferencia As Date) As Array

    Local aAreaSE2   As Array
    Local aDadosRef  As Array
    Local aRecnos    As Array
    Local dDataIni   As Date
    Local dDataFim   As Date 
    Local cModTot    As Character
    Local lBaseSE2	 As Logical 
    Local lIRPFBaixa As Logical
    Local lCalcIssBx As Logical
    Local lLojaAtu   As Logical
    Local lCalcPa 	 As Logical
    Local lPCCBaixa  As Logical
    Local aStruct    As Array
    Local aCampos    As Array
    Local cAliasQry  As Character
    Local cQuery     As Character
    Local nLoop      As Numeric
    Local lTodasFil  As Logical 
    Local aFil10925  As Array
    Local lTodosFor	 As Logical 
    Local aFornece	 As Array
    Local cLayout    As Character
    Local lGestao    As Logical
    Local cFilFwSE2  As Character
    Local cTipoIn 	 As Character
    Local lContinua  As Logical 
    Local aPercPcc   As Array
    Local cVencPub   As Character
    Local lEmpPub    As Logical
    Local nVencto 	 As Numeric

    Default __lIsIssBx := FindFunction("IsIssBx")

    aAreaSE2   := SE2->( GetArea() )
    aDadosRef  := Array( 5 )
    aRecnos    := {}
    dDataIni   := FirstDay( dReferencia )
    dDataFim   := LastDay( dReferencia )
    cModTot    := GetNewPar( "MV_MT10925", "1" )
    lBaseSE2   := __lLocBRA .And. SuperGetMv("MV_BS10925",.T.,"1") == "1"
    lIRPFBaixa := IIf( __lLocBRA, SA2->A2_CALCIRF == "2", .F.)
    lCalcIssBx := IIF(__lIsIssBx, IsIssBx("P"), SuperGetMv("MV_MRETISS",.F.,"1") == "2" )
    lLojaAtu   := .F.
    lCalcPa    := .F.
    lPCCBaixa  := SuperGetMv("MV_BX10925",.T.,"2") == "1"
    aStruct    := {}
    aCampos    := {}
    cAliasQry  := ""
    cQuery     := ""
    nLoop      := 0
    lTodasFil  := ExistBlock("FA050RTF")
    aFil10925  := {}
    lTodosFor  := ExistBlock("FA050FOR")
    aFornece   := {}
    cLayout    := ""
    lGestao    := .F.
    cFilFwSE2  := ""
    cTipoIn    := MVABATIM + "|" + MV_CPNEG + "|" + MVPROVIS + "|" + MVPAGANT
    lContinua  := .T.
    aPercPcc   := {}
    cVencPub   := SuperGetMV("MV_VENPUB", .F., "M")
    lEmpPub    := IsEmpPub()
    nVencto    :=  SuperGetMv("MV_VCPCCP",.T.,1) 

    If M->E2_TIPO $ MVPAGANT
        lCalcPa := .T.
    EndIf

    aDadosRef := Iif( Type("aDadosRef") != "A", Array(7), aDadosRef )
    aDadosRet := Iif( Type("aDadosRet") != "A", Array(7), aDadosRet )
    aDadosImp := Iif( Type("aDadosImp") != "A", Array(3), aDadosImp )

    AFill( aDadosRef, 0 )
    AFill( aDadosImp, 0 )

    If M->E2_APLVLMN <> "2" //Se verifica o valor minimo de retencao do PCC

        //Verifico todas as filiais apenas quando SA2 compartilhado
        If lTodasFil
            aFil10925 := ExecBlock( "FA050RTF", .F., .F. )
        Else
            aFil10925 := { cFilant }
        Endif

        If lTodosFor
            aFornece := ExecBlock("FA050FOR", .F., .F.)
            If ValType(aFornece) <> "A"
                lTodosFor := .F.
            Endif
        Endif

        If lEmpPub .And. !lIRPFBaixa .And. !lPCCBaixa	
            If cVencPub == "D"
                dDataIni   := dReferencia
                dDataFim  := dReferencia
            ElseIf cVencPub == "M"
                dDataIni := FirstDay(dDataIni)
                dDataFim := LastDay(dDataIni)
            EndIF            
	    EndIF
        SE2->( dbCommit() )

        cQuery := "SELECT E2_VALOR,E2_PIS,E2_COFINS,E2_EMISSAO,E2_CSLL,E2_ISS,E2_INSS,E2_IRRF,E2_VRETPIS,E2_VRETCOF,E2_VRETCSL,E2_PRETPIS,E2_PRETCOF,E2_PRETCSL,E2_VRETIRF,E2_NATUREZ,"
        cQuery += "E2_BASEPIS,E2_BASECOF,"
        cQuery += "E2_APLVLMN,"
        cQuery += "E2_PREFIXO,E2_NUM,E2_PARCELA,E2_TIPO, R_E_C_N_O_ RECNO FROM "
        cQuery += RetSqlName( "SE2" ) + " SE2 "
        cQuery += "WHERE "

        //--- Tratamento Gestao Corporativa
        cLayout     := FWSM0Layout()
        lGestao	    := "E" $ cLayout .Or. "U" $ cLayout
        cFilFwSE2   := IIF( lGestao , FwFilial("SE2") , xFilial("SE2") )

        If Len(aFil10925) == 1 .Or. Empty( cFilFwSE2 ) //Compartilhado
            cQuery += "E2_FILIAL='" + xFilial("SE2") + "' AND "
        Else //Multifiliais
            cQuery += "( "
            For nLoop := 1 to Len(aFil10925)
                cQuery += "E2_FILIAL ='" + aFil10925[nLoop] + "' OR "
            Next
            //Retiro o ultimo OR
            cQuery := Left( cQuery, Len( cQuery ) - 4 )
            cQuery += ") AND "
        Endif
        FwFreeArray(aFil10925)

        //Verifica se utiliza o ponto de Entrada FA050FOR para analise de imposto atraves de outras filiais SA2
        If ! lTodosFor
            cQuery += "E2_FORNECE = '"+M->E2_FORNECE+"' AND "
            lLojaAtu := ( GetNewPar( "MV_LJ10925", "1" ) == "1" )
            If lLojaAtu  //Considero apenas a loja atual
                cQuery += "E2_LOJA = '"+M->E2_LOJA+"' AND "
            Endif
        Else
            cQuery += "( "
            For nLoop := 1 To Len(aFornece)
                cQuery += "E2_FORNECE = '"+aFornece[nLoop,1]+"' AND E2_LOJA = '"+aFornece[nLoop,2]+"'"+IIF(nLoop < Len(aFornece)," OR "," ")
            Next
            cQuery += ") AND "
            FwFreeArray(aFornece)
        Endif

        If lPccBaixa
            cQuery += "E2_VENCREA >= '" + DToS( dDataIni )      + "' AND "
            cQuery += "E2_VENCREA <= '" + DToS( dDataFim )      + "' AND "

            cQuery += "E2_TIPO NOT IN " + F050TipoIN(cTipoIn,.T.) 	  + " AND "
        Else 
            If nVencto == 2
				 cQuery += "E2_VENCREA >= '" + DToS( dDataIni )      + "' AND "
                 cQuery += "E2_VENCREA <= '" + DToS( dDataFim )      + "' AND "
			ElseIf nVencto == 1 .OR. EMPTY(nVencto)
                 cQuery += "E2_EMISSAO >= '" + DToS( dDataIni )      + "' AND "
                 cQuery += "E2_EMISSAO <= '" + DToS( dDataFim )      + "' AND "
			ElseIf nVencto == 3
                cQuery += "E2_EMIS1 >= '" + DToS( dDataIni )      + "' AND "
                cQuery += "E2_EMIS1 <= '" + DToS( dDataFim )      + "' AND "
			Endif
        
            If lCalcPa
                cQuery += "E2_TIPO IN " + F050TipoIN(MVPAGANT,.T.) + " AND "
            Else
                cQuery += "E2_TIPO NOT IN " + F050TipoIN(cTipoIn,.T.) 	  + " AND "
            EndIf
            
        EndIf

        //se aplico o valor minimo, devo considerar apenas os titulos nesta situacao para verificacao
        //de retencoes anteriores, pendencias de retencao etc.
        //Titulos em que E2_APLVLMN = 2 (nao aplica o valor minimo de R$ 5000) nao compoem a base do PCC
        cQuery += "	E2_APLVLMN = '1' AND "
        cQuery += "(E2_DESDOBR <> 'S' OR ( E2_DESDOBR = 'S' AND E2_STATUS <> 'D')) AND " // não considera titulo pai de desdobramento para imposto
        cQuery += "D_E_L_E_T_=' '"

        cQuery := ChangeQuery( cQuery )
        cAliasQry := GetNextAlias()

        dbUseArea( .T., "TOPCONN", TcGenQry(,,cQuery), cAliasQry, .F., .T. )

        aCampos := { "E2_VALOR", "E2_IRRF", "E2_ISS", "E2_INSS", "E2_PIS", "E2_COFINS", "E2_CSLL", "E2_VRETPIS", "E2_VRETCOF", "E2_VRETCSL" }
        aStruct := SE2->( dbStruct() )

        For nLoop := 1 To Len( aStruct )
            If !Empty( AScan( aCampos, AllTrim( aStruct[nLoop,1] ) ) )
                TcSetField( cAliasQry, aStruct[nLoop,1], aStruct[nLoop,2], aStruct[nLoop,3], aStruct[nLoop,4] )
            EndIf
        Next nLoop
        FwFreeArray(aStruct)
        FwFreeArray(aCampos)

        While !( cAliasQRY )->( Eof())

            If BuscaSE5( "BA", ( cAliasQRY )->E2_PREFIXO, ( cAliasQRY )->E2_NUM, ( cAliasQRY )->E2_PARCELA, ( cAliasQRY )->E2_TIPO, "DSD" )
                //Se o titulo for Pai do desdobramento, desconsiderar no calculo de imposto
                ( cAliasQRY )->( dbSkip() )
                Loop
            EndIf

            // Desconsidera se o titulo encontrado for o mesmo que está sendo alterado
            If lAltera .And. ( cAliasQRY )->RECNO == SE2->( Recno() )
                ( cAliasQRY )->( dbSkip() )
                Loop
            EndIf

            //Armazeno os valores calculados por titulo.
            If ( cAliasQRY )->E2_PIS > 0 .And. ( !lPccBaixa .Or. (lPccBaixa .And. (cAliasQRY)->E2_PRETPIS <> "2") )
                aDadosImp[1] += ( cAliasQRY )->E2_PIS
            EndIf

            If ( cAliasQRY )->E2_COFINS > 0 .And. ( !lPccBaixa .Or. (lPccBaixa .And. (cAliasQRY)->E2_PRETCOF <> "2") )
                aDadosImp[2] += ( cAliasQRY )->E2_COFINS
            EndIf

            If ( cAliasQRY )->E2_CSLL > 0 .And. ( !lPccBaixa .Or. (lPccBaixa .And. (cAliasQRY)->E2_PRETCSL <> "2") )
                aDadosImp[3] += ( cAliasQRY )->E2_CSLL
            EndIf

            If cModTot == "1"
            	lContinua := .T.
            Else
            	lContinua := !Empty( ( cAliasQRY )->E2_PIS ) .Or. !Empty( ( cAliasQRY )->E2_COFINS ) .Or. !Empty( ( cAliasQRY )->E2_CSLL )
            EndIf

            If lContinua

                //Verifico se utiliza a base de imposto ou o valor do titulo
                //para totalizar a retencao no mes
                If !lBaseSe2
                    adadosref[1] += ( ( cAliasQRY )->E2_VALOR + Iif(lCalcIssBx, 0,( cAliasQRY )->E2_ISS) + ( cAliasQRY )->E2_INSS + Iif(lIRPFBaixa, 0,( cAliasQRY )->E2_IRRF) )
                Else
                    If Empty( ( cAliasQRY )->E2_BASEPIS )
                        If cModTot == "1"
                        	adadosref[1] += ( ( cAliasQRY )->E2_VALOR + ( cAliasQRY )->E2_ISS + ( cAliasQRY )->E2_INSS + ( cAliasQRY )->E2_IRRF )
                        Else
                        	aDadosRef[1] += ( ( cAliasQRY )->E2_VALOR + Iif(lCalcIssBx, 0,( cAliasQRY )->E2_ISS) + ( cAliasQRY )->E2_INSS + Iif(lIRPFBaixa, 0,( cAliasQRY )->E2_IRRF) )
                        EndIf
                    Else
                        adadosref[1] += (cAliasQRY)->E2_BASEPIS
                    Endif
                Endif

                If Empty( ( cAliasQRY )->E2_PRETPIS )
                    If !lBaseSE2 .Or. Empty( ( cAliasQRY )->E2_BASEPIS )
                        // se foi atribuido pela basePIS então já está com o valor cheio, senao deve recompor os impostos no valor
                        aDadosRef[1] += Iif( Empty( ( cAliasQRY )->E2_VRETPIS ), ( cAliasQRY )->E2_PIS, ( cAliasQRY )->E2_VRETPIS )
                    EndIf
                    //Armazeno os valores calculados por titulo, retirando os valores retidos
                    If ( cAliasQRY )->E2_VRETPIS + ( cAliasQRY )->E2_VRETCOF + ( cAliasQRY )->E2_VRETCSL + Iif(lIRPFBaixa, ( cAliasQRY )->E2_VRETIRF , 0 ) > 0
                        aDadosImp[1] -= (cAliasQRY)->E2_VRETPIS
                    Endif
                EndIf

                If Empty( ( cAliasQRY )->E2_PRETCOF )
                    If !lBaseSE2 .Or. Empty( ( cAliasQRY )->E2_BASEPIS )
                        // se foi atribuido pela basePIS então já está com o valor cheio, senao deve recompor os impostos no valor
                        aDadosRef[1] += Iif( Empty( ( cAliasQRY )->E2_VRETCOF ), ( cAliasQRY )->E2_COFINS, ( cAliasQRY )->E2_VRETCOF )
                    EndIf
                    //Armazeno os valores calculados por titulo, retirando os valores retidos
                    If ( cAliasQRY )->E2_VRETPIS + ( cAliasQRY )->E2_VRETCOF + ( cAliasQRY )->E2_VRETCSL + Iif(lIRPFBaixa, ( cAliasQRY )->E2_VRETIRF , 0 ) > 0
                        aDadosImp[2] -= (cAliasQRY)->E2_VRETCOF
                    Endif
                EndIf

                If Empty( ( cAliasQRY )->E2_PRETCSL )
                    If !lBaseSE2 .Or. Empty( ( cAliasQRY )->E2_BASEPIS )
                        // se foi atribuido pela basePIS então já está com o valor cheio, senao deve recompor os impostos no valor
                        aDadosRef[1] += Iif( Empty( ( cAliasQRY )->E2_VRETCSL ), ( cAliasQRY )->E2_CSLL, ( cAliasQRY )->E2_VRETCSL )
                    EndIf
                    //Armazeno os valores calculados por titulo, retirando os valores retidos
                    If ( cAliasQRY )->E2_VRETPIS + ( cAliasQRY )->E2_VRETCOF + ( cAliasQRY )->E2_VRETCSL + Iif(lIRPFBaixa, ( cAliasQRY )->E2_VRETIRF , 0 ) > 0
                        aDadosImp[3] -= (cAliasQRY)->E2_VRETCSL
                    Endif
                EndIf

                If ( !Empty( ( cAliasQRY )->E2_PIS ) .Or. !Empty( ( cAliasQRY )->E2_COFINS ) .Or. !Empty( ( cAliasQRY )->E2_CSLL ) )  ;
                .And. ( Empty( ( cAliasQRY )->E2_VRETPIS ) .Or. Empty( ( cAliasQry )->E2_VRETCOF ) .Or. Empty( ( cAliasQry )->E2_VRETCSL ) ) ;
                .And. ( ( cAliasQRY )->E2_PRETPIS == "1" .Or. ( cAliasQry )->E2_PRETCOF == "1" .Or. ( cAliasQry )->E2_PRETCSL == "1" )

                    If Empty( ( cAliasQRY )->E2_VRETPIS ) .And. ( cAliasQRY )->E2_PRETPIS == "1"
                        aDadosRef[2] += ( cAliasQRY )->E2_PIS
                    EndIf

                    If Empty( ( cAliasQRY )->E2_VRETCOF )	.And. ( cAliasQRY )->E2_PRETCOF == "1"
                        aDadosRef[3] += ( cAliasQRY )->E2_COFINS
                    EndIf

                    If Empty( ( cAliasQRY )->E2_VRETCSL ) .And. ( cAliasQRY )->E2_PRETCSL == "1"
                        aDadosRef[4] += ( cAliasQRY )->E2_CSLL
                    EndIf 

                    AAdd( aRecnos, ( cAliasQRY )->RECNO ) 
                EndIf     
                    
                If ( ( Empty((cAliasQRY)->E2_PIS) .And. !Empty((cAliasQRY)->E2_BASEPIS) ) .Or. ( Empty((cAliasQRY)->E2_COFINS) .And. !Empty((cAliasQRY)->E2_BASECOF) ) ;
                .Or. ( Empty( (cAliasQRY)->E2_CSLL ) .And. !Empty( (cAliasQRY)->E2_CSLL) )    )  ;
                .And. ( Empty( (cAliasQRY)->E2_VRETPIS) .Or. Empty( (cAliasQry)->E2_VRETCOF) .Or. Empty( (cAliasQry)->E2_VRETCSL)  ) ;
                .And. ( Empty((cAliasQRY)->E2_PRETPIS) .Or. Empty((cAliasQry)->E2_PRETCOF) .Or. Empty((cAliasQry)->E2_PRETCSL) ) 

                    aPercPcc := GetPerPCC( (cAliasQRY)->E2_NATUREZ ) 

                    If ALTERA 

                         If Empty((cAliasQRY)->E2_PRETPIS) 
                            aDadosImp[1] += ( cAliasQRY )->E2_BASEPIS * aPercPcc[1]
                        EndIf

                        If Empty((cAliasQRY)->E2_PRETCOF) 
                            aDadosImp[2] += ( cAliasQRY )->E2_BASEPIS * aPercPcc[2]
                        EndIf

                        If Empty((cAliasQRY)->E2_PRETCSL) 
                            aDadosImp[3] += ( cAliasQRY )->E2_BASEPIS * aPercPcc[3]
                        EndIf

                    Else 

                        If Empty((cAliasQRY)->E2_PRETPIS) 
                            aDadosRef[2] += ( cAliasQRY )->E2_BASEPIS * aPercPcc[1]
                        EndIf

                        If Empty((cAliasQRY)->E2_PRETCOF) 
                            aDadosRef[3] += ( cAliasQRY )->E2_BASEPIS * aPercPcc[2]
                        EndIf

                        If Empty((cAliasQRY)->E2_PRETCSL) 
                            aDadosRef[4] += ( cAliasQRY )->E2_BASEPIS * aPercPcc[3]
                        EndIf

                    EndIf                    

                EndIf     

            Endif
            ( cAliasQRY )->( dbSkip() )
        EndDo

        // Fecha a area de trabalho da query
        ( cAliasQRY )->( dbCloseArea() )
        dbSelectArea( "SE2" )
    Endif

    aDadosRef[5] := AClone(aRecnos)

    RestArea(aAreaSE2)
    FwFreeArray(aAreaSE2)
    FwFreeArray(aRecnos)

Return( aDadosRef )

//-------------------------------------------------------------------
/*/{Protheus.doc} F050CalcRt
Esta rotina tem como objetivo verificar se o somatorio das
parcelas eh total de duplicatas do documento de Entrada

@author Sergio Silveira
@since  17/08/2004
/*/
//-------------------------------------------------------------------
Function F050CalcRt()

    Local cAcessRad:= GetNewPar( "MV_AC10925", "1" )

    Local nRadio   := Val(cModRetPis)
    Local nOpca    := 0
    Local oDlgRet
    Local oRadio
    Local oBold
    Local oBmp
    Local oBut1
    Local oBut2
    Local nRadioOld := nRadio

    //Base de imposto Variavel
    Local lBaseImp	:= F050BSIMP(2)	//Verifica a existência dos campos e o calculo de impostos

    //Somente permito a alteracao quando os dados estiverem preenchidos
    If Empty(M->E2_VALOR) .OR. Empty(M->E2_NATUREZ) .OR. Empty(M->E2_FORNECE) .OR. Empty(M->E2_LOJA) .OR. ;
        Empty(M->E2_VENCREA)
        Return( .T. )
    Endif

    DEFINE MSDIALOG oDlgRet TITLE STR0127 FROM 09,0 TO 25.8,60 OF oMainWnd 	//"Calculo de retencao"

    DEFINE FONT oBold NAME "Arial" SIZE 0, -13 BOLD

    @  0, -25 BITMAP oBmp RESNAME "PROJETOAP" oF oDlgRet SIZE 55, 1000 NOBORDER WHEN .F. PIXEL

    @ 03, 40 SAY STR0125 FONT oBold PIXEL // "Modalidade de retencao do PIS/COFINS/CSLL"

    @ 14, 30 TO 16 ,400 LABEL '' OF oDlgRet   PIXEL

    @ 25, 40 RADIO oRadio VAR nRadio 3D SIZE 70, 11 PROMPT STR0128,STR0129,STR0130 of oDlgRet PIXEL  //"Calculado pelo sistema"###"Efetua retencao"###"Nao efetua retencao"

    oRadio:SetEnable( cAcessRad == "1" )

    DEFINE SBUTTON oBut1 FROM 100, 169 TYPE 1 ACTION ( nOpca := 1, oDlgRet:End() )  ENABLE of oDlgRet
    DEFINE SBUTTON oBut2 FROM 100, 202 TYPE 2 ACTION ( nOpca := 0, oDlgRet:End() )  ENABLE of oDlgRet

    ACTIVATE MSDIALOG oDlgRet CENTERED

    If nOpca == 1
        cModRetPis := Str( nRadio, 1 )
        If nRadio != nRadioOld
            Do Case
                Case nRadio == 1
                    Fa050Natur()
                Case nRadio == 2
                    Fa050Natur(,,,.T.)
                Case nRadio == 3
                    M->E2_VALOR += M->E2_PIS + M->E2_COFINS + M->E2_CSLL
                    If lBaseImp .and. m->e2_basepis > 0
                        FVerMinImp(m->e2_basepis,.T.)
                    Else
                        FVerMinImp(m->e2_valor,.T.)
                    Endif
                    M->E2_VLCRUZ:=Round(NoRound(xMoeda(M->E2_VALOR,M->E2_MOEDA,1,M->E2_EMISSAO,MsDecimais(1)+1,M->E2_TXMOEDA),MsDecimais(1)+1),MsDecimais(1))
            EndCase
        Endif
    EndIf

Return( .T. )

//-------------------------------------------------------------------
/*/{Protheus.doc} f050VerVlr
Verifica se o valor será menor que zero

@param nValorTit, numeric, Valor de referência usado no cálculo do PCC
@return Nil

@author Mauricio Pequim Jr
@since 17/08/2004
@type function
/*/
//-------------------------------------------------------------------
Static Function f050VerVlr(nValorTit)

    Local nTotARet	:= 0
    Local nSobra 	:= 0
    Local nValLiq	:= 0

    // Guarda os valores originais
    nValLiq := nValorTit - M->E2_IRRF - M->E2_ISS - M->E2_INSS - M->E2_SEST - M->E2_PIS - M->E2_COFINS - M->E2_CSLL
    nDifPcc := 0

    If nValLiq < 0
        nTotARet := M->E2_PIS + M->E2_COFINS + M->E2_CSLL
        nSobra := nValorTit - nTotARet

        If nSobra < 0
            nFatorRed := 1 - ( Abs( nSobra ) / nTotARet )

            M->E2_PIS  := NoRound( M->E2_PIS * nFatorRed, 2 )
            M->E2_COFINS := NoRound( M->E2_COFINS * nFatorRed, 2 )
            M->E2_CSLL := nValorTit - ( M->E2_PIS + M->E2_COFINS ) - 0.01

            If lAltera
                nDifPCC := nSobra
            Endif
        Endif
    EndIf

Return

//-------------------------------------------------------------------
/*/{Protheus.doc} FCalcIr
Calculo do IRRF de Pessoa Fisica	e Juridica

@nBaseIrrf = Base Irrf
@cTipo = Tipo de Pessoa (Juridica ou Fisica)
@lFinanceiro = Indica que o calculo foi chamado pelo modulo Financeiro
@lIrfRetAnt = Controle de retencao anterior no mesmo periodo
@lSRefSE2 = Define se a pesquisa deve acontecer sem a referencia de um titulos na SE2
@lComiss = Define se a rotina de pagto comissoes chamou esta funcao

@author Mauricio Pequim Jr
@since  11/08/04
/*/
//-------------------------------------------------------------------
Function FCalcIr(nBaseIrrf As Numeric, cTipo As Character, lFinanceiro As Logical, lIrfRetAnt As Logical, lSRefSE2 As Logical, lComiss As Logical, lRegra As Logical) As Numeric

    Local aArea			:= GetArea()
    Local aFilial		:= {}
    Local aCliFor		:= {}
    Local aAreaSED      := SED->(GetArea())
    Local aStru         := SE2->(dbStruct())
    Local nTotTit		:= 0
    Local nTotInss		:= 0
    Local nTotIrRtd 	:= 0
    Local nValor		:= 0
    // Carrega variavel de verificacao de consideracao de valor minimo de retencao de IR.
    Local lAplMinIR 	:= .F.
    Local nVenctoPF 	:= SuperGetMv("MV_ACMIRPF",.T.,"3")  //1 = Emissao    2= Vencimento Real	3=Data Contabilizacao
    Local nVenctoPJ 	:= SuperGetMv("MV_ACMIRPJ",.T.,"3")  //1 = Emissao    2= Vencimento Real	3=Data Contabilizacao
    Local lVencto	 	:= .F.
    Local lCalcIr	 	:= .F.
    Local lNatIr	 	:= .F.
    //Controla o Pis Cofins e Csll na baixa
    Local lPCCBaixa 	:= SuperGetMv("MV_BX10925",.T.,"2") == "1"
    // Controla IRPF na Baixa
    Local lIRPFBaixa    := IIf( __lLocBRA, SA2->A2_CALCIRF == "2", .F.)
    Local nTotRtIr		:= 0
    Local nRecAtual		:= SE2->(RECNO())
    Local lNumDep		:= .T.
    Local nBaseDep		:= GetMV("MV_TMSVDEP",,0)
    Local cAglImPJ		:= SuperGetMv("MV_AGLIMPJ",.T.,"1")
    Local cQuery		:= ""
    Local nLoop			:= 0
    Local nBaseSED		:= 1
    Local cArqTmp		:= ""
    Local nInssRA		:=	0
    Local lPaBruto		:= GetNewPar("MV_PABRUTO","2") == "1"  //Indica se o PA terá o valor dos impostos descontados do seu valor
    //Variavel indica se irá provisionar os impostos de INSS e ISS na inclusão da PA, deduzindo-os do valor de adiantamento.
    Local lPrImPA       := !lPaBruto .And. SuperGetMv("MV_PAPRIME",.T.,"2") == "1"
    Local nValtit       := 0
    Local lDedInSS      := (SuperGetMv("MV_INSIRF",.F.,"2") == "1" .and. SED->ED_BASEIRC == 0) .And. cTipo != "J"
    Local nTotInCar     := 0
    Local lDelTrbIR     := .T.
    Local nX 		    := 0
    Local bSEDCompart	:= { || Empty(xFilial("SED")) }
    Local lGestao		:= TamSx3("E5_FILIAL")[1] > 2
    Local cFilFwSA2     := IIF( lGestao , FwFilial("SA2") , xFilial("SA2") )
    Local lF050Auto2	:= Iif( TYPE("lF050Auto") == "U" , .F. , lF050Auto )
    Local lBaseDif		:= cPaisLoc $ "ANG|ARG|AUS|BOL|BRA|CHI|COL|COS|DOM|EQU|EUA|HAI|PAD|PAN|PAR|PER|POR|PTG|SAL|TRI|URU|VEN"
    Local lDesdob		:= lFinanceiro .And. SE2->E2_DESDOBR == "S"
    Local cAcmIrrf 		:= SuperGetMv("MV_ACMIRRF",.T.,"1")  //1 = Acumula    2= Não acumula
    Local nNroFil 		:= 0
    Local lE2FilComp    := FwModeAccess("SE2",3) == "C"
    Local nMoeda		:= 0
    Local nTxMoeda		:= 0
    Local dEmiss		:= dDataBase
    Local lRtInssPA		:= .F.
    Local cCodRet		:= ""
    Local cTipoIn       := MVABATIM+"|"+MV_CPNEG+"|"+MVPROVIS
    Local nTamFat       := TamSX3("E2_FATURA")[1]
    Local dDataQry      := CTOD("//")
    Local lFIN050IR 	:= Existblock("FIN050IR")
    Local lAcumIr		:= ExistBlock("F050CALIR")
    Local lF50CIRFF		:= ExistBlock("F50CIRFF")
    Local nPercIrrf     := 0
    Local nDedBase 		:= 0
    Local lSubstPr      := IIf(Type("lSubst") <> "U" , lSubst , .F.) .And. FWIsInCallStack("FA050Subst")  // Veriricação se esta sendo realizado Substitução de Provisorio
    Local cIdDoc        As Char
    Local cChaveTit     As Char
    Local dVigMP1171    As Date
    Local nBasOrig      As Numeric
    Local nVrIrDedS     As Numeric   
    Local lIrTabSimp    As Logical
    
    DEFAULT nBaseIrrf	:= 0   //valor vem sempre na moeda do título exceto comissão que é sempre na moeda corrente
    DEFAULT cTipo		:= "F" //Pessoa Fisica
    DEFAULT lFinanceiro	:= .F. //Indica que o calculo foi chamado pelo modulo Financeiro
    DEFAULT lIrfRetAnt	:= .F. //Controle de retencao anterior no mesmo periodo
    DEFAULT lSRefSE2	:= .F. //Define se a pesquisa deve acontecer sem a referencia de um titulos na SE2
    DEFAULT lComiss		:= .F. //Define se a rotina de pagto comissoes chamou esta funcao
	DEFAULT lRegra		:= .T. //Define se ao calcular o IR, se ira considerar as regras da rotina Complemento de Impostos
    DEFAULT __lNRasDSD  := SuperGetMV("MV_NRASDSD",.T.,.F.)

    cIdDoc    := ""
    cChaveTit := ""
    nBasOrig  := 0
    nVrIrDedS := 0
    lIrTabSimp := SuperGetMV("MV_FMP1171",.F.,.F.) //Habilita calculo do IRPF c/ dedução simplificada
    dVigMP1171 := CTOD("01/05/2023") //Inicio da vigencia da MP 1.171/23

    INCLUI:= IF(Type("INCLUI") == "U", .T., INCLUI)
    ALTERA:= IF(Type("ALTERA") == "U", .F., ALTERA)

    If __lLocBRA
        __lFlagFKF := FKF->(ColumnPos("FKF_REINF")) > 0
    Endif

    If __lLocBRA
        __lDedSimpl := .F.
    Endif

    __lRateioIR := .F.
    lRatOk   := IF(Type("lRatOk") != "L", .T., lRatOk)

    If	IsBlind()
        If Type("M->E2_INSS")=="U"
            If SE2->E2_INSS>0
                nInssRA	:= SE2->E2_INSS
            Endif
        Elseif	M->E2_INSS > 0
            nInssRA	:= M->E2_INSS
        Endif
    Endif

    If FunName() = "FINA050" .or. lF050Auto2
        lSRefSE2	:=	.F.
    Endif

    //Ponto de entrada para verificar se Acumula ou não os valores de IR no calculo
    //Alteracao efetuada para atender a Pinheiro Neto Advogados
    If lAcumIr
        lCalcIr := ExecBlock("F050CALIR",.F.,.F.)
    EndIf

    // Verifica se o fornecedor trata o valor minimo de retencao.
    // 1 - Não considera  2 - Considera o parâmetro MV_VLRETIR
    If __lLocBRA .and. SA2->A2_MINIRF == "2"
        lAplMinIR := .T.
    Endif

    //Se for Gestao utilizo outra funcao para verificar filial compartilhada
    If __lCodFil .And. lGestao
        bSEDCompart := { ||  Empty(FWFilial("SED")) }
    Endif

    If !lFinanceiro .AND. !lSRefSE2
        RegToMemory("SE2",.F.,.F.)
    Endif

    If lFinanceiro .AND. !lSRefSE2
        If IsBlind()
            If Type("M->E2_NUM") == "U"
                RegToMemory("SE2",.F.,.F.)
            Endif
        Else
            RegToMemory("SE2",.F.,.F.,,Funname())
        Endif

        nLastDay := Day(LastDay(M->E2_EMISSAO))
        nTamData := Iif(Len(Dtoc(M->E2_EMISSAO)) == 10, 7, 5)
        dDataImp := M->E2_EMISSAO
    Else
        nLastDay := Day(LastDay(dDataBase))
        nTamData := IIf(Len(DtoC(dDataBase)) == 10, 7, 5)
        dDataImp := dDataBase
        //Ignorar a configuracao do parametro MV_ACMIRPF, forcando a cumulatividade trabalhar com a database
        //ja que nao ha referencia do SE2 quanto a emissao ou vencimento real
        nVenctoPF := "3"
    Endif

    //Valida se a natureza corrente calcula IR
    If lFinanceiro
        //natureza do Financeiro é validadada antes da chamada desta função
        lNatIr := .T.
        If SED->ED_RINSSPA == "1"
            lRtInssPA := .T.
        EndIf

        //---------------------------------------------------------------------------------------
        // Conversao das bases para moeda corrente
        // valor vem sempre na moeda do título exceto comissão que é sempre na moeda corrente
        // aqui convertemos para moeda um pois os impostos serão todos na moeda corrente.
        If lComiss
            nMoeda := 1
            dEmiss := dDatabase
            nTxMoeda := 1
        Else
            nMoeda	:= If( Type("M->E2_MOEDA")=="U", SE2->E2_MOEDA, M->E2_MOEDA )
            dEmiss	:= If( Type("M->E2_EMISSAO")=="U", SE2->E2_EMISSAO, M->E2_EMISSAO )
            nTxMoeda := If( Type("M->E2_TXMOEDA")=="U", SE2->E2_TXMOEDA, M->E2_TXMOEDA )

            If nBaseIrrf == 0
                If Type("M->E2_VALOR")=="U"
                    nBaseIrrf := SE2->E2_VALOR
                Else
                    nTotTit := M->E2_VALOR
                EndIf
            Endif
            nBaseIrrf := xMoeda(nBaseIrrf,nMoeda,1,dEmiss,MsDecimais(1)+1,nTxMoeda)
        Endif
    Else
        //Se não, desconsidera retenção de pendências (Caso não seja do Financeiro)
        dbSelectArea("SED")
        aAreaSED := SED->(GetArea())
        SED->(dbSetOrder(1))
        If SED->(dbSeek(xFilial("SED")+M->E2_NATUREZ))
            If SED->ED_CALCIRF == "S"
                lNatIr := .T.
            Endif
            If SED->ED_RINSSPA == "1"
                lRtInssPA := .T.
            EndIf
        Endif
        SED->(RestArea(aAreaSED))
    Endif

    If lNatIr
        //Verifico a combinacao de filiais (SM0) e lojas de fornecedores a serem considerados
        //na montagem da base do IRRF
        If cAglImPJ != "1"
            aRet := FLOJASIRRF("2")
            aFilial := aClone(aRet[1])
            aCliFor := aClone(aRet[2])
            cArqTMP := aRet[3]
        Endif

        If lFinanceiro .and. cTipo == "F"
            f050CRatIR(lIRPFBaixa)
        EndIf

        //Acumula o valor do IRRF retido anteriormente
        If (cAcmIrrf	==	"1" .Or. cTipo == "F") .And. (!lAcumIr .Or. (lAcumIr .and. lCalcIr))

            cQuery := "SELECT DISTINCT SE2.E2_FILIAL,SE2.E2_PREFIXO,SE2.E2_NUM,SE2.E2_PARCELA,SE2.E2_TIPO,SE2.E2_FORNECE,SE2.E2_LOJA, "
            cQuery += "SE2.E2_EMIS1,SE2.E2_VENCREA,SE2.E2_EMISSAO,SE2.E2_NATUREZ,SE2.E2_VALOR,SE2.E2_IRRF,SE2.E2_INSS,SE2.E2_ISS,SE2.E2_FATURA, "
            cQuery += "SE2.E2_ORIGEM, SE2.E2_SALDO, SE2.E2_DESDOBR, SE2.E2_MOEDA, SE2.E2_TXMOEDA, SE2.E2_VLCRUZ, SE2.E2_FILORIG "

            If __lLocBRA
                cQuery += ",SE2.E2_SEST, SE2.E2_BASEIRF"
            Endif

            cQuery += ",SE2.E2_PRETPIS,SE2.E2_PRETCOF,SE2.E2_PRETCSL,SE2.E2_VRETPIS,SE2.E2_VRETCOF,SE2.E2_VRETCSL "
            cQuery += ",SE2.E2_VRETIRF, SE2.E2_PRETIRF, SE2.E2_FILORIG "
            cQuery += ",SED.ED_BASEIRC, SED.ED_BASEIRF, SED.ED_IRRFCAR "

            cQuery += "FROM " + RetSQLname("SE2") + " SE2, "
            cQuery +=           RetSQLname("SED") + " SED "
            cQuery += " WHERE "

            nNroFil := Len(aFilial)

            //Se verifica base apenas na filial corrente e fornecedor corrente
            If cAglImPJ == "1"
                If lE2FilComp
                    cQuery += "SE2.E2_FILORIG = '" + cFilAnt + "' AND "
                Else
                    cQuery += "SE2.E2_FILIAL = '" + xFilial("SE2") + "' AND "
                EndIf

                cQuery += "SE2.E2_FORNECE = '" + SA2->A2_COD + "' AND "
                cQuery += "SE2.E2_LOJA = '" + SA2->A2_LOJA + "' AND "
            ElseIf nNroFil > 0
                If Empty( cFilFwSA2 )
                    cQuery += If(lE2FilComp, "SE2.E2_FILORIG IN( ", "SE2.E2_FILIAL IN ( ")

                    For nLoop := 1 to nNroFil
                        cQuery += "'" + aFilial[nLoop] + "',"
                    Next nLoop

                    //Retiro a ultima virgula
                    cQuery := Left(cQuery, Len(cQuery) - 1)
                    cQuery += ") AND "

                    //Verificar determinados fornecedores (raiz do CNPJ)
                    cQuery += " (E2_FORNECE||E2_LOJA IN (SELECT CODIGO||LOJA FROM " + cArqTMP + ")) AND "

                Else//Se cadastro de Clientes EXCLUSIVO
                    cQuery += " (E2_FILIAL||E2_FORNECE||E2_LOJA IN (SELECT FILIALX||CODIGO||LOJA FROM " + cArqTMP + ")) AND "
                Endif
            Endif

            // Para Pessoa fisica totaliza os titulos emitidos no mes
            If cTipo == "F"
                If nVenctoPF == "2"
                    dDataQry := If(Type("M->E2_VENCREA")=="U", SE2->E2_VENCREA, M->E2_VENCREA )
                    cQuery += "SE2.E2_VENCREA  BETWEEN '" + Dtos(FirstDay(dDataQry)) + "' AND '" + Dtos(LastDay(dDataQry)) + "' AND "
                    lVencto := .T.

                ElseIf nVenctoPF == "1"
                    dDataQry := If(Type("M->E2_EMISSAO")=="U", SE2->E2_EMISSAO, M->E2_EMISSAO )
                    cQuery += "SE2.E2_EMISSAO  BETWEEN '" + Dtos(FirstDay(dDataQry)) + "' AND '" + Dtos(LastDay(dDataQry)) + "' AND "

                Else // nVenctoPF == "3"
                    cQuery += "SE2.E2_EMIS1  BETWEEN '" + Dtos(FirstDay(dDataBase)) + "' AND '" + Dtos(LastDay(dDataBase))+ "' AND "
                Endif
            Else
                // Para Pessoa juridica totaliza os titulos emitidos no dia
                If nVenctoPJ == "2"
                    dDataQry := If(Type("M->E2_VENCREA")=="U", SE2->E2_VENCREA, M->E2_VENCREA )
                    cQuery += "SE2.E2_VENCREA  = '" + Dtos(dDataQry) + "' AND "	//Totaliza pelo vencimento real
                    lVencto := .T.

                ElseIf nVenctoPJ == "1"
                    dDataQry := If(Type("M->E2_EMISSAO")=="U", SE2->E2_EMISSAO, M->E2_EMISSAO )
                    cQuery += "SE2.E2_EMISSAO  = '" + Dtos(dDataQry) + "' AND "

                ElseIf nVenctoPJ == "3" .OR. EMPTY(nVenctoPJ)
                    cQuery += "SE2.E2_EMIS1  = '" + Dtos(dDataBase) + "' AND "
                Endif
            Endif
            cQuery += "SE2.E2_TIPO NOT IN " + F050TipoIN(cTipoIn,.T.) + " AND "
            cQuery += "(SE2.E2_FATURA = '"+ Space(nTamFat) + "' OR "
            cQuery += "SE2.E2_FATURA <> '"+ PADR('NOTFAT',nTamFat) + "') AND "
            cQuery += "SE2.E2_STATUS <> 'D' AND "  //Desconsidera os titulos geradores de desdobramento
            If !lRegra
                cQuery += "SE2.R_E_C_N_O_ <> "+Str(nRecAtual)+"  AND "
            EndIf
            cQuery += "SE2.D_E_L_E_T_ = ' ' AND "
         
            //Verifico a filial do SED
            If cAglImPJ == "1" .or. (Eval(bSEDCompart)) //SED Compartilhado
                cQuery += "SED.ED_FILIAL = '"+ xFilial("SED") + "' AND "
            Elseif len(aFilial) >0
                cQuery += "SED.ED_FILIAL IN ( "
                For nLoop := 1 to Len(aFilial)
                    cQuery += "'"  + aFilial[nLoop] + "',"
                Next
                //Retiro a ultima virgula
                cQuery := Left( cQuery, Len( cQuery ) - 1 )
                cQuery += ") AND "
            Endif

            cQuery += "SE2.E2_NATUREZ = SED.ED_CODIGO AND "

       		cCodRet := If(Type("M->E2_CODRET")=="U", SE2->E2_CODRET, M->E2_CODRET)
    		cQuery += "SE2.E2_CODRET = '" + cCodRet + "' AND "
            cQuery += "SED.ED_CALCIRF = 'S' AND "
            If __lLocBRA
            	cQuery += "SED.ED_JURCAP <> '1' AND "
            EndIf
            cQuery += "SED.D_E_L_E_T_ = ' ' "

            If lFIN050IR
                cQuery += ExecBlock("FIN050IR", .F. , .F. , cQuery)
            Endif

            cQuery := ChangeQuery(cQuery)

            dbUseArea(.T., "TOPCONN", TCGenQry(,,cQuery), "TRBIRF", .F., .T.)

            For nX := 1 to Len(aStru)
                If aStru[nX,2] != 'C' .And. FieldPos(aStru[nX,1]) > 0 // Se existir o campo na Query
                    TCSetField('TRBIRF', aStru[nX,1], aStru[nX,2],aStru[nX,3],aStru[nX,4])
                Endif
            Next

            dbSelectArea("TRBIRF")
            While !(TRBIRF->(Eof()))
                //Se for inclusao, somo todos os titulos
                //Se for alteração, somo todos os titulos exceto o que esta sendo alterado.
                If ((INCLUI .Or. lSubstPr) .AND. lFinanceiro) .OR. ((ALTERA .OR. !lFinanceiro) .and. SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)!= ;
                    TRBIRF->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA))
                    
                    cChaveTit:= xFilial("SE2", TRBIRF->E2_FILORIG ) + "|" + TRBIRF->E2_PREFIXO + "|" + TRBIRF->E2_NUM + "|" + TRBIRF->E2_PARCELA + "|" + TRBIRF->E2_TIPO + "|" + TRBIRF->E2_FORNECE + "|" + TRBIRF->E2_LOJA
                    cIdDoc := FINBuscaFK7(cChaveTit, "SE2")

                    //Desconsiderar titulo originador de desdobramento
                    dbSelectArea("FI8")
                    FI8->(DbSetOrder(1))
                    If !__lNRasDSD .AND. (dbSeek(xFilial("FI8")+TRBIRF->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)) .or.;
                        __cChTitDs == xFilial("SE2")+TRBIRF->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA))
                        TRBIRF->(dbSkip())
                        Loop
                    Endif

                    //Tratamento efetuado para tratar E2_BASEIRF sem gravação de redução de base, pois o ajuste da gravação E2_BASEIRF, ocorreu em Mar/2013
                    nValTit := TRBIRF->(E2_VLCRUZ)
                    nValtit += TRBIRF->(E2_IRRF + E2_INSS + E2_ISS + E2_SEST)

                    If !lPccBaixa .And. TRBIRF->(E2_PRETPIS == " " .And. E2_PRETCOF == " " .And. E2_PRETCSL == " ")
                        nValtit	+= TRBIRF->(E2_VRETPIS+E2_VRETCOF+E2_VRETCSL)
                    Endif

                    If __lLocBRA .and. TRBIRF->ED_BASEIRC > 0 .AND. TRBIRF->(E2_BASEIRF) <> nValtit
                        nTotTit	+= TRBIRF->(E2_BASEIRF)
                    Else
                        If lBaseDif .and. TRBIRF->ED_BASEIRF > 0
                            nTotTit	+= nValtit*(TRBIRF->ED_BASEIRF/100)
                        Elseif __lLocBRA .and. TRBIRF->ED_BASEIRC > 0
                            nTotTit	+= nValtit*(TRBIRF->ED_BASEIRC/100)
                        ElseIf __lLocBRA .and. TRBIRF->(E2_BASEIRF) <> nValtit
                            nTotTit	+= TRBIRF->(E2_BASEIRF)               
                        Else
                            nTotTit	+= nValtit
                        Endif
                    Endif

                    If !lDedInSS .and. TRBIRF->ED_IRRFCAR== "S"
                        nTotInCar+= TRBIRF->E2_INSS
                    Else
                        nTotInss += TRBIRF->E2_INSS
                    Endif

                    nTotIrRtd += TRBIRF->E2_IRRF
                    If __lFlagFKF .and. FindFunction("F986Deduz") .AND. __lLocBRA
                        nTotTit	-= F986Deduz(cIdDoc,"IRF")
                    EndIf

                    //Soma os valores que deveriam ter sido retidos
                    //Retidos e os pendentes (menor que valor minimo)
                    nTotRtIr += TRBIRF->E2_VRETIRF

                    If __lRateioIR
                        If Len(__oRatIRF:aRatIRF) > 1
                            lRatOK := f050RatLeg(TRBIRF->E2_FILORIG,TRBIRF->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA))
                            
                            //Verifica o IR retido por CPF (Rateio de Ir Progressivo)
                            If lRatOK
                                __oRatIRF:GetIdDoc(TRBIRF->(E2_FILIAL + "|" + E2_PREFIXO + "|" + E2_NUM + "|" + E2_PARCELA + "|" + E2_TIPO + "|" + E2_FORNECE + "|" + E2_LOJA))
                                __oRatIRF:GetIRRetido(__oRatIRF:cIdDoc)
                            EndIf
                        Else
                            __oRatIRF:SetIRRetido(TRBIRF->E2_IRRF,1) 
                        Endif            
					EndIf

                    If (lAcumIr .and. !lCalcIr) .Or. cAcmIrrf	==	"2"
                        nTotRtIr := 0
                    Endif
                Endif
                TRBIRF->(dbSkip())
            Enddo
        EndIf

        If Select("TRBIRF") > 0
            dbSelectArea("TRBIRF")
            dbCloseArea()
        Endif

        //Quando a rotina for utilizada pelo Financeiro, tenho a necessidade de calcular o IRRF do titulo presente
        //Nao ocorre com o Compras pois o mesmo já efetuou calculos
        If lFinanceiro
            If !lSRefSE2 .and. (cTipo != "F" .Or. (__lLocBRA .And. cTipo == "F" .And. SED->ED_JURCAP=="1"))
                nTotTit := nBaseIrrf
            Else
                nTotTit += nBaseIrrf
            Endif

            nBasOrig := nTotTit //Guarda a base sem as deduções legais para o calculo do IRPF simplificado

            If Type("M->E2_INSS")=="U"
                If !(SuperGetMv("MV_INSIRF",.F.,"2") == "1") .and. SED-> ED_IRRFCAR== "S"
                    nTotInCar += SE2->E2_INSS
                Else
                    nTotInss += SE2->E2_INSS
                Endif
            Else
                If !lDedInSS .and. SED-> ED_IRRFCAR== "S"
                    nTotInCar+= M->E2_INSS
                Else
                    nTotInss += M->E2_INSS
                Endif
            Endif

            If nTotInss == 0 .And. IsBlind() .And. nInssRA > 0
                nTotInss	:=	nInssRA
            Endif

            //Com o controle de retenção de IRRF, nao necessito somar titulo a titulo
            //Apenas calcular o IRRF do titulo presente
            //nBaseIrrf > 0 -> Base pre definida pelo SIGAPLS
            If lBaseDif .and. SED->ED_BASEIRF > 0 .and. cTipo != "F"
                nBaseSED := SED->ED_BASEIRF/100
            Endif

            If cTipo != "F" .and. nBaseIrrf == 0
                If !lSRefSE2
                    If Type("M->E2_VALOR")=="U"
                        nBaseIrrf := IIF(lDedInSS, (SE2->E2_VALOR * nBaseSED) - SE2->E2_INSS , (SE2->E2_VALOR * nBaseSED) )
                    Else
                        nBaseIrrf := IIF(lDedInSS, (m->e2_valor * nBaseSED) - m->e2_inss , (m->e2_valor * nBaseSED) )
                    EndIf
                Else
                    nBaseIrrf := IIF(lDedInSS, nTotTit - nTotInss , nTotTit)
                Endif
            Else
                nTotTit -= nTotInCar
                nBaseIrrf := IIF(lDedInSS, nTotTit - nTotInss , nTotTit)
                lParc := cTipo == "F" .And. lDesdob .And. FwIsInCallStack("GeraParcSe2")
                If  lPrImPA .and.  Type("M->E2_TIPO")<>"U" .and. !lRtInssPA
                    If M->E2_TIPO $ MVPAGANT
                        nBaseIrrf := IIF(lDedInSS, nTotTit - M->E2_PRINSS , nTotTit)
                    EndIf
                EndIf
            Endif
        Endif

        //Fecha arquivo temporario
        If cAglImPJ != "1" .and. lDelTrbIR .and. (UPPER(Alltrim(TCGetDb()))!="POSTGRES")
            If InTransact()
                StartJob( "DELTRBIR" , GetEnvServer()  , .T. , SM0->M0_CODIGO, FWCODFIL(),.T.,ThreadID(),cArqTmp)//,TCGetDb())
            Else
                DELTRBIR(SM0->M0_CODIGO, FWCODFIL(),.F.,0,cArqTmp,TCGetDb())
            Endif
        Endif
        dbSelectArea("SE2")

    Else
        If lFinanceiro
            If !lSRefSE2
                nTotTit := nBaseIrrf
            Else
                nTotTit += nBaseIrrf
            Endif

            nTotInss  := If( Type("M->E2_INSS")=="U" , SE2->E2_INSS , M->E2_INSS )

            If lBaseDif .and. SED->ED_BASEIRF > 0
                nBaseSED := SED->ED_BASEIRF / 100
            Endif
            nBaseIrrf := IIF(lDedInSS, (nTotTit * nBaseSED) - nTotInss , (nTotTit * nBaseSED) )
        Endif
    EndIf

    If lFinanceiro
        //Abato os dependentes dos Fornecedores Pessoa Fisica
        If lNumDep .and. cTipo == "F"
            nBaseIrrf -= (nBaseDep * SA2->A2_NUMDEP)
        Endif

        //Ponto de entrada para manipulacao da base de calculo.
        //Deve ser utilizado para tratamento de reducao da base por numero de dependentes
        //Retornar Base de Calculo - SEMPRE NA MOEDA CORRENTE
        IF lF50CIRFF
            nBaseIrrf := ExecBlock("F50CIRFF",.f.,.f.,nBaseIrrf)
        Endif

        If cPaisLoc=="BRA" .and. lRegra
            nDedBase  := Fa986regra("SE2","IRF","1" )
        EndIf

        nBaseIrrf := nBaseIrrf + nDedBase
        If nBaseIrrf < 0
            nBaseIrrf := 0
        EndIf
        //Calculo o IRRF devido no periodo
        If cTipo == "F" 
            If __lRateioIR .and. lRatOK            
                __oRatIRF:SetBaseIR(nBaseIrrf, nBasOrig)
                nValor := __oRatIRF:CalcRatIr()
            Else
                //Calculo IRPF considerando deduções legais
                nValor := Round(NoRound(fa050TabIR(Round(nBaseIrrf,MsDecimais(1))),3),2)
                //Calculo do IRPF considerando dedução simplificada (MP 1.171/23)
                If cPaisLoc=="BRA" .And. lIrTabSimp .And. dEmiss >= dVigMP1171 .And. SED->ED_JURCAP <> '1'
                    nVrIrDedS := Round(NoRound(fa050TabIR(Round(nBasOrig,MsDecimais(1)),,lIrTabSimp),3),2) 
                    If nValor > nVrIrDedS 
                        nValor := nVrIrDedS //Considera o IRRF c/ dedução simplificada por ser mais vantajoso
                        __lDedSimpl := .T.
                    EndIf
                EndIf
            EndIf 
        Else
            nPercIrrf := IIF(SED->ED_PERCIRF > 0, SED->ED_PERCIRF, GetMV("MV_ALIQIRF"))
            nValor 	:= nBaseIrrf  * nPercIrrf / 100

            If GetNewPar("MV_RNDIRF",.F.)
                nValor	:= Round(nValor,2)
            Else
                nValor	:= NoRound(nValor,2)
            EndIf
        Endif
    Else
        dbSelectArea("SE2")
        SE2->(dbGoto(nRecAtual))
        nValor := SE2->E2_IRRF
    Endif

    //Se verifico a retencao atraves de campo
    //Guardo o valor que deveria ser retido
    //Atualizo o valor pendente de retencao mais o IRRF do titulo
    //Se for chamada pelo rotina de pagamento de comissões, nao faz tratamento de reclock
    //pois a função MT530Nat() está dentro de transaction.
    If __lLocBRA .And. !lComiss
        // Se nao for IR na baixa, grava o valor retido de IR
        // caso contrario o campo deve ser gravado gradativamente
        // a cada baixa
        If !lIRPFBaixa
            If lRegra //Deixa de gravar caso seja simulacao do calculo para gravar base na FKF
                If Type("M->E2_NUM")=="U"
                    RecLock("SE2",.F.)
                    SE2->E2_VRETIRF	:= nValor
                    SE2->( MsUnLock() )
                Else
                    M->E2_VRETIRF	:= nValor
                Endif
            Endif

            If (SED->ED_JURCAP <> '1')
                If cTipo == "F"  //Pessoa Fisica
                    If SA2->A2_TIPO =="F"
                        nValor -= Iif( __lRateioIR, 0, nTotIrRtd )  //Diminuo do valor calculado, o IRRF já retido
                        If lRegra
                            M->E2_VRETIRF	:= nValor
                        Endif
                    Else
                        nValor -= Iif( __lRateioIR, 0, nTotIrRtd )
                    Endif
                ElseIf cAcmIrrf	<>	"2"
                    nValor += nTotRtIr - nTotIrRtd
                Endif
            Endif
        EndIf
    EndIf

    //Controle de retencao anterior no mesmo periodo
    lIrfRetAnt := IIF(nTotIrRtd > 0, .T., .F.)

    //No calculo de IR pela baixa, nao se aplica valor minimo de retencao quando IR
    lAplMinIr := IIF(lIRPFBaixa, .F., lAplMinIr)

    // Verifica se o fornecedor trata o valor minimo de retencao.- FINANCEIRO
    If (lFinanceiro .and. lAplMinIR .And. (nValor <= GetMv("MV_VLRETIR") .and. !lIrfRetAnt)) .OR. nValor < 0
        nValor := 0
    Endif
    If __lLocBRA .and. cTipo == "F" .and. (!__lRateioIR .or. !lFinanceiro)
        f050LRatIR(.T.)
    EndIf

    If __oRatQry != Nil
        __oRatQry:Destroy()
        FwFreeObj(__oRatQry)
        __oRatQry := Nil
    EndIf    
    RestArea(aArea)

    FwFreeArray( aArea )
    FwFreeArray( aFilial )
    FwFreeArray( aCliFor )
    FwFreeArray( aAreaSED )
    FwFreeArray( aStru )

Return nValor

//-------------------------------------------------------------------
/*/{Protheus.doc} Fa050Filho
Verifica se existe titulo filho (titulo de taxa) e se este sofreu
baixa

@Return lRet - Permite exclusão do título filho

@author Claudio Donizete de Souza
@since  11/08/04
/*/
//-------------------------------------------------------------------
Function Fa050Filho(lVerBaixa)

    Local lRet := .T.
    Local aFilhos := {}
    Local nX
    Local aAreaSe2 := SE2->(GetArea())
    Local lRotBaixa := FwIsInCallStack("FINA080")
    Local lCb10925 := GetNewPar("MV_CB10925","2") == "1" // Define se permite ou não o cancelamento de títulos pais que possuem títulos filhos de impostos baixados. 1=Permite cancelar 2=Não permite
	Local lCancBaixa := IsInCallStack("FA080CAN")

    Default lVerBaixa := .F.

    aFilhos := ImpCtaPg()

    For nX := 1 to Len(aFilhos)

        SE2->( DbGoTo(aFilhos[nX,9]) ) // Posiciono no Título filho

        If !lRotBaixa // Processo via Emissão

            If !lCb10925

                // Se encontrou o titulo filho (titulo de tributo) e este sofreu baixa
                // Não permite a exclusao do titulo pai (titulo principal).
                If !lVerBaixa .Or. SE2->E2_SALDO != SE2->E2_VALOR
                    lRet := .F.
                    Exit
                Endif

            EndIf
        Else
            If !lVerBaixa .Or. SE2->E2_SALDO != SE2->E2_VALOR
                If !( Alltrim(SE2->E2_ORIGEM) == "FINA241" .and. lCancBaixa )
                    lRet := .F.
                Endif
                Exit
            Endif
        EndIf

    Next

    SE2->(RestArea(aAreaSe2))

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} Fa050FDarf
Verifica se existe titulo filho (titulo de taxa) e se este está em
alguma DARF

@author Adrianne Furtado Andrade
@since  15/07/09
/*/
//-------------------------------------------------------------------
Function Fa050FDarf(lHelp)

    Local lRet := .T.
    Local lDARF := .F.
    Local aAreaSe2 := SE2->(GetArea())
    Local cTitPai := SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)

    Default lHelp := .T.

    lDARF := ExstDarfPg(SE2->E2_FILIAL, cTitPai, lHelp)

    If lDARF // Caso exista DARF retorno é falso
        lRet := .F.
    EndIf

    SE2->(RestArea(aAreaSe2))

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} F050GrvFI2
Grava a tabela FI2 com as justificativas

@author Marcelo Pimentel
@since  23/11/05
/*/
//-------------------------------------------------------------------
Function F050GrvFI2(lGravaLog)

    Local nX		:=	1
    Local aArea 	:= GetArea()
    Local cJustific	:= ""
    Local lCposJust := CposJust()
    DEFAULT lGravaLog := .F.
    If lGravaLog
        lCposJust := .T.
    EndIf

    If lCposJust .and. Type('aSE2FI2') == "A" .And. Len(aSE2FI2) > 0
        FI2->(DbSetOrder(3))
        For nX:=1 To Len(aSE2FI2)
            If !Empty(aSE2FI2[nX][2])
                cChave	:=	xFilial("FI2")+"2"+SE2->(E2_NUMBOR+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)+aSE2FI2[nX][1]+"2"
                // Pesquisa pela ocorrencia nao gerada (FI2_GERADO = 2 - Ja esta na chave)
                If FI2->(DbSeek(cChave))
                    RecLock('FI2',.F.)
                    Replace FI2_DTGER WITH dDataBase
                    MsUnLock()
                Else
                    RecLock('FI2',.T.)
                    Replace FI2_FILIAL 	WITH xFilial("FI2")
                    Replace FI2_CARTEI  WITH "2"
                    Replace FI2_GERADO  WITH "2"
                    Replace FI2_NUMBOR 	WITH SE2->E2_NUMBOR
                    Replace FI2_PREFIX	WITH SE2->E2_PREFIXO
                    Replace FI2_TITULO	WITH SE2->E2_NUM
                    Replace FI2_PARCEL	WITH SE2->E2_PARCELA
                    Replace FI2_TIPO  	WITH SE2->E2_TIPO
                    Replace FI2_CODFOR	WITH SE2->E2_FORNECE
                    Replace FI2_LOJFOR	WITH SE2->E2_LOJA
                    Replace FI2_DTOCOR	WITH dDataBase
                    Replace FI2_VALANT	WITH aSE2FI2[nX][3]
                    Replace FI2_VALNOV	WITH aSE2FI2[nX][4]
                    Replace FI2_CAMPO 	WITH aSE2FI2[nX][5]
                    Replace FI2_TIPCPO	WITH aSE2FI2[nX][6]
                    Replace FI2_SEQ  	WITH aSE2FI2[nX][1]
                    cJustific := __CUSERID + " -  " + Dtoc(dDatabase) + " / " + Substr(Time(),1,5) + If( _Opc==3,STR0135,STR0136 ) 	//" - Inclusao " ### " - Alteracao "
                    MsUnLock()

                    MSMM(FI2_HISTOR,,,cJustific+aSE2FI2[nX][2],1,,,"FI2","FI2_HISTOR")
                Endif
            Endif
        Next nX
    Endif
    RestArea(aArea)

Return(.T.)

//-------------------------------------------------------------------
/*/{Protheus.doc} Fa050JUST
Historico para Alteracao em titulos e inclusoes do tipo AB
Abatimento e Alteracao

@author Marcelo Pimentel
@since  23/11/05
/*/
//-------------------------------------------------------------------
Function Fa050JUST()

    Local lRet			:= .T.
    Local nMaxLenAnt	:= 10
    Local nMaxLenAtu	:= 10
    Local nOpcA			:= 0
    Local cTipoTit		:= M->E2_TIPO
    Local oGetDados
    Local lCposJust := CposJust()
    Private aHead	:= {}

    If _Opc == 3
        //Na inclusao verificar se o tipo do titulo pertence a AB-
        If cTipoTit <> "AB-"
            Return .T.
        Endif
    EndIf

    //Rotina automatica nao abre tela de justificativa
    If lF050Auto
        Return .T.
    Endif
    If lCposJust
        If Empty(aSE2FI2)
            //Para inclusao de titulos do tipo abatimento.
            If Empty(aCposAlter)
                AADD(aCposAlter,"E2_TIPO")
            EndIF

            aSE2FI2	:= BuildSE2FI2( aCposAlter )
        Endif

        If Len(aSE2FI2) > 0
            aHead	:=	{}
            AAdd(aHead, {STR0137,"SEQ","",2 ,0,,Nil,"C",,,,,".F.",,,,})				//"Seq"
            AAdd(aHead, {STR0134,"HISTOR" ,"",80,0,,Nil,"M",,,,,"FA050MEMO()",,,,})	//"Justificativa"
            AAdd(aHead, {STR0138,"VALANT","",nMaxLenAnt,0,,Nil,"C",,,,,".F.",,,,})	//"Valor Anterior"
            AAdd(aHead, {STR0139,"VALATU","",nMaxLenAtu,0,,Nil,"C",,,,,".F.",,,,})	//"Valor Atual"
            AAdd(aHead, {STR0140,"NOMCPO","",10,0,,Nil,"C",,,,,".F.",,,,})			//"Nome Campo "
            AAdd(aHead, {STR0141,"TIPCPO","",1 ,0,,Nil,"C",,,,,".F.",,,,})			//"Tipo Campo "
            DEFINE FONT oBold NAME "Arial" SIZE 0, -13 BOLD
            DEFINE MSDIALOG oDlg FROM 88 ,22  TO 350,619 TITLE STR0062 Of oMainWnd PIXEL	//"Histórico"
            @ 14 ,03   TO 40 ,296 LABEL '' OF oDlg PIXEL
            @ 19 ,10   SAY STR0142 Of oDlg PIXEL SIZE 280 ,30 FONT oBold COLOR CLR_BLUE		//"A justificativa deverá conter no mínimo 5 caracteres."

            oGetDados := MsNewGetDados():New(45,3,120,296,IIf(_Opc==2,0,GD_UPDATE),"fa050juLok","fa050juTOk",,,,Len(aSE2FI2),,,,oDlg,aHead,aSE2FI2)

            ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar(oDlg,{||nOpca:=1,If(oGetDados:TudoOk(),oDlg:End(),nOpca := 0)},{||oDlg:End()})

            If nOpca==1
                aSE2FI2:=aClone(oGetDados:aCols)
            Else
                lRet := .F.
            EndIf
        EndIf
    Endif

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} BuildSE2FI2
Retorna os dados do array aSE2FI2

@aCpos = nomes dos campos que serao avaliados
@author Marcelo Pimentel
@since  23/11/05
/*/
//-------------------------------------------------------------------
Static Function BuildSE2FI2( aCpos )

    Local aItems 		:= {}
    Local nMaxLenAnt	:=	10
    Local nMaxLenAtu	:=	10
    Local nX			:=	0
    Local cDadoAnt		:=	""
    Local cDadoAtu		:=	""
    Local xValAnt		:=	""
    Local cTipo  		:=  ""
    Local cRet			:=  ""
    Local cItems		:=  ""
    Local lCposJust 	:= CposJust()
    Local lF050JUST 	:= ExistBlock("F050JUST")

    If lCposJust
        //Monta Interface para incluir as JUSTIFICATIVAS
        dbSelectArea("FI2")
        DbSetOrder(3)
        If DbSeek(xFilial("FI2")+"2"+M->E2_NUMBOR+M->E2_PREFIXO+M->E2_NUM+M->E2_PARCELA+M->E2_TIPO+M->E2_FORNECE+M->E2_LOJA)
            While !Eof() .and. FI2->FI2_NUMBOR+FI2->FI2_PREFIX+FI2->FI2_TITULO+FI2->FI2_PARCEL+FI2->FI2_TIPO+FI2->FI2_CODFOR+FI2->FI2_LOJFOR == ;
            M->E2_NUMBOR+M->E2_PREFIXO+M->E2_NUM+M->E2_PARCELA+M->E2_TIPO+M->E2_FORNECE+M->E2_LOJA .AND. ;
            xFilial("FI2") == FI2->FI2_FILIAL
                cRet := MSMM(FI2->FI2_HISTOR,80)
                Aadd(aItems,{FI2->FI2_SEQ,cRet,FI2->FI2_VALANT,FI2->FI2_VALNOV,FI2->FI2_CAMPO,FI2->FI2_TIPCPO,.F.})
                dbSkip()
            EndDo
        EndIf

        If lF050JUST
            aItems := ExecBlock("F050JUST",.f.,.f.,{aItems})
        Else
            For nX := 1 To Len(aCpos)
                xValAnt	:=	&("M->"+aCpos[nX])
                If xValAnt <> SE2->(FieldGet(FieldPos(aCpos[nX])))
                    cTipo	:=	ValType(SE2->(FieldGet(FieldPos(aCpos[nX]))))
                    Do Case
                        Case  cTipo == "L"
                        cDadoAnt	:=	AlltoChar(SE2->(FieldGet(FieldPos(aCpos[nX]))))
                        cDadoAtu	:=	AlltoChar(xValAnt)
                        Case  cTipo == "D"
                        cDadoAnt	:=	DtoC(SE2->(FieldGet(FieldPos(aCpos[nX]))))
                        cDadoAtu	:=	DtoC(xValAnt)
                        Case  cTipo == "N"
                        cDadoAnt	:=	TransForm(SE2->(FieldGet(FieldPos(aCpos[nX]))),PesqPict('SE2',aCpos[nX]))
                        cDadoAtu	:=	TransForm(xValAnt,PesqPict('SE2',aCpos[nX]))
                        OtherWise
                        cDadoAnt	:=	SE2->(FieldGet(FieldPos(aCpos[nX])))
                        cDadoAtu	:=	xValAnt
                    EndCase
                    cItems	:= StrZero(Len(aItems)+1,2)
                    Aadd(aItems,{cItems," " ,cDadoAnt,cDadoAtu,aCpos[nX],cTipo,.F.})
                    nMaxLenAnt	:=	Max(nMaxLenAnt,Len(cDadoAnt))
                    nMaxLenAtu	:=	Max(nMaxLenAtu,Len(cDadoAtu))
                Endif
            Next
        EndIf
    Endif
Return aItems

//-------------------------------------------------------------------
/*/{Protheus.doc} fa050juLok
Valida a linha da getdados

@author Marcelo Pimentel
@since  23/11/05
/*/
//-------------------------------------------------------------------
Function fa050juLok()

    Local nPosMemo := Ascan(aHead, {|x| x[2] == "HISTOR"})
    // Nao valida linha deletada
    If aCols[n][Len(aHead)+1]
        Return .T.
    Endif

    If Empty(aCols[n][nPosMemo])
        Aviso( STR0026, STR0143, {"Ok"} )		//"Atencao" ### "Obrigatório o preenchimento da justificativa."
        Return .F.
    Endif

    If Len(aCols[n][nPosMemo]) < 5
        Aviso( STR0026, STR0144, {"Ok"} )		//"Atencao" ### "Justificativa inválida, favor redigitá-la."
        Return .F.
    Endif

Return .T.

//-------------------------------------------------------------------
/*/{Protheus.doc} fa050juTOk
Valida todas as linhas da getdados

@author Marcelo Pimentel
@since  23/11/05
/*/
//-------------------------------------------------------------------
Function fa050juTOk()

    Local lRet := .T.
    Local nX   := 0

    For nX := 1 To Len(aCols)
        If !fa050juLok()
            lRet := .F.
            Exit
        Endif
    Next

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} FA050MEMO
Valida se campo MEMO para modo de alteracao ou visualizacao

@author Marcelo Pimentel
@since  23/11/05
/*/
//-------------------------------------------------------------------
Function FA050MEMO()

    Local lRet	:= .T.
    FI2->(dbsetorder(3))
    If FI2->(DbSeek(xFilial("FI2")+"2"+M->E2_NUMBOR+M->E2_PREFIXO+M->E2_NUM+M->E2_PARCELA+M->E2_TIPO+M->E2_FORNECE+M->E2_LOJA+StrZero(n,2)))
        lRet := .F.
    EndIf

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} CposJust
Valida existencia dos campos para justificativa

@author Mauricio Pequim Jr
@since  29/11/05
/*/
//-------------------------------------------------------------------
Function CposJust()

    Local lCposJust := .T.
    //Liga Justificativa Instrucoes Bancarias
    // 1 = Receber
    // 2 = Pagar
    // 3 = Liga Ambas
    // 4 = Nao Utiliza
    Local lJustCP   := SuperGetMv("MV_INCOBBC",.T.,"4") $ "2/3"

Return lCposJust .And. lJustCP

//-------------------------------------------------------------------
/*/{Protheus.doc} F050PcoLan
Executa validacaso de saldos do PCO

@author Bruno Sobieski
@since  19/01/06
/*/
//-------------------------------------------------------------------
Function F050PcoLan()

    Local lRet	:=	.T.
    If !PcoVldLan("000002",IIF(M->E2_TIPO$MVPAGANT,"02","01"),"FINA050")
        lRet	:=	.F.
        // Grava os lancamentos nas contas orcamentarias SIGAPCO
        If SE2->E2_TIPO $ MVPAGANT
            PcoDetLan("000002","02","FINA050")
        Else
            PcoDetLan("000002","01","FINA050")
        EndIf
    Endif

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} Fa050bAval
Avalia se o titulo pode ser seleciona na substituicao de
titulos provisorios

@author Claudio Donizete
@since  27/03/06
/*/
//-------------------------------------------------------------------
Function Fa050bAval(cMarca,oValor,oQtdtit,nValorS,nQtdTit,oMark,nMoedSubs,aChaveLbn,nRegSel)
    Local lRet 		:= .T.
    Local cChaveLbn

    cChaveLbn := "SUBS" + xFilial("SE2")+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA
    // Verifica se o registro nao esta sendo utilizado em outro terminal
    //-- Parametros da Funcao LockByName() :
    //   1o - Nome da Trava
    //   2o - usa informacoes da Empresa na chave
    //   3o - usa informacoes da Filial na chave
    If LockByName(cChaveLbn,.T.,.F.)
        Fa050Inverte(cMarca,oValor,oQtdtit,@nValorS,@nQtdTit,@oMark,nMoedSubs,aChaveLbn,cChaveLbn,.F.,@nRegSel) // Marca o registro e trava
        lRet := .T.
    Else
        IW_MsgBox(STR0147,STR0115,"STOP") // "Este titulo está sendo utilizado em outro terminal, não pode ser utilizado para substituição"##Atenção
        lRet := .F.
    Endif
    oMark:oBrowse:Refresh(.t.)
Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} MenuDef
Utilizacao de menu Funcional

Parametros do array aRotina
1. Nome a aparecer no cabecalho
2. Nome da Rotina associada
3. Reservado
4. Tipo de Transação a ser efetuada:
	1 - Pesquisa e Posiciona em um Banco de Dados
	2 - Simplesmente Mostra os Campos
	3 - Inclui registros no Bancos de Dados
	4 - Altera o registro corrente
	5 - Remove o registro corrente do Banco de Dados
5. Nivel de acesso
6. Habilita Menu Funcional

@author Ana Paula N. Silva
@version P12
@since   29/11/2006
@return  Array com opcoes da rotina.
/*/
//-------------------------------------------------------------------
Static Function MenuDef(nOpcion)

    Local aRotina
    Local lF050ROT 	:= ExistBlock("F050ROT")

    Default __lF50CMNT := FindFunction("F050CMNT")
    Default __lIntPFS  := SuperGetMv("MV_JURXFIN",.T.,.F.) //Integração do Financeiro com o Juridico(Habilitado = .T.)
    Default __lFIN50VA := FindFunction("FINA050VA")
    
    If cPaisLoc $ "ARG|POR|EUA" .And. nOpcion # Nil
        aRotina := {{ STR0001 ,"FA050PesInd" , 0 , 1, ,.F.},; //"Pesquisar"
        			{ STR0002 ,"FA050Visua"	 , 0 , 2},; //"Visualizar"
                    { STR0003 ,"FA050Inclu"	 , 0 , 3},; //"Incluir"
                    { STR0004 ,"FA050Alter"	 , 0 , 4},; //"Alterar"
                    { STR0005 ,"FA050Delet"	 , 0 , 5},; //"Excluir"
                    { STR0133 ,"MSDOCUMENT"	 , 0 , 4},; //"Conhecimento"
                    { STR0252 ,"FN50Log"   	 , 0 , 7},; //"historico do titulo"
                    { STR0098 ,"FA040Legenda", 0 , 6 ,,.F.} } //"Legenda"
    Else
        aRotina := {{ STR0001 ,"AxPesqui"    , 0 , 1, ,.F.},; //"Pesquisar"
        			{ STR0002 ,"FA050Visua"  , 0 , 2},; //"Visualizar"
                    { STR0003 ,"FA050Inclu"  , 0 , 3},; //"Incluir"
                    { STR0004 ,"FA050Alter"  , 0 , 4},; //"Alterar"
                    { STR0005 ,"FA050Delet"  , 0 , 5},; //"Excluir"
                    { STR0006 ,"FA050Subst"  , 0 , 6},; //"Substituir"
                    { STR0165 ,"FaCanDsd"    , 0 , 5},; //"Canc.Desdobr."
                    { STR0110 ,"FA050Rateio" , 0 , 2},; //"Vis Rateio"
                    { STR0159 ,"FA050AGEND"  , 0 , 6},; //Agendamento
                    { STR0133 ,"MSDOCUMENT"  , 0 , 4},;//"Conhecimento"
                    { STR0098 ,"FA040Legenda", 0 , 7,,.F.},;// "Legenda"
                    { STR0252 ,"FN50Log"     , 0 , 9} }// "historico do titulo"
    Endif

    aAdd( aRotina, { STR0231 ,"CTBC662"   , 0, 8}) //"Tracker Contábil"
    aAdd( aRotina, { STR0187 ,"FA050Docs" , 0, 6}) //"Documentos"
    aAdd( aRotina, { STR0201 ,"FA050Contr", 0, 2}) // Rastr. Contratos
    aAdd( aRotina, { STR0247 ,"FINA689"   , 0, 4}) //"Conversão em Lote de Adtos Viagem"

	If cPaisLoc == "RUS"
		aAdd( aRotina, { STR0311 ,"FIN50PQBrw('PR')",0,4})		//"Track PRs"
		aAdd( aRotina, { STR0317 ,"FIN50PQBrw('PO')",0,4})		//"Track POs"
		aAdd( aRotina, { STR0323 ,"FIN50PQBrw('BS')",0,4})      //"Bank Statements"
	EndIf

    If __lLocBRA
	    aAdd( aRotina, { STR0279,"FINA986('SE2',.T.)",0,4}) //"Complemento do ti­tulo"
    EndIf

    aAdd( aRotina, { STR0366 ,"FinWizFac('SE2')",0, 4, 2, .F.}) //"Facilitador"

    // Consulta multinatureza
    If __lF50CMNT .and. MV_MULNATP
        aAdd( aRotina, { STR0277 ,"F050CMNT()", 0 , 2})	//"Consulta Rateio Multi Naturezas - Emissão"
    Endif

    If __lIntPFS //Integração SIGAPFS x SIGAFIN
        aAdd(aRotina, {STR0296, "JURA246(4,,,, .T.)", 0, 0, 0, NIL }) //"Detalhe / Desdobramentos" (M¢dulo SIGAPFS)
        aAdd( aRotina , { STR0299, "JURA247(4)", 0, 0, 0, NIL } ) //"Desdobramento Pós Pagamento"
        If FindFunction("JURA273")
            aAdd( aRotina, { STR0349, "JURA273()", 0, 0, 0, NIL } ) // "Copiar Título"
        EndIf
    Endif

    //Motor de Retenções
    If __lLocBRA
        aAdd( aRotina, { STR0300  ,"FINCRET('SE2')"   , 0, 9}) //'Consulta de Retenções'
    Endif

    If __lFIN50VA 
        aAdd( aRotina, {  STR0313 , "FINA050VA", 0, 4 } ) //###STR0313 "Valores Acessórios"
    EndIf

    //Ponto de entrada para inclusão de novos itens no menu aRotina
    If lF050ROT
        aRotinaNew := ExecBlock("F050ROT",.F.,.F.,aRotina)
        If (ValType(aRotinaNew) == "A")
            aRotina := aClone(aRotinaNew)
        EndIf
    EndIf

Return(aRotina)

//-------------------------------------------------------------------
/*/{Protheus.doc} F050CmpRat
Retorna Campos do Rateio

@author Adrianne Furtado
@since  13/12/06
/*/
//-------------------------------------------------------------------
Function F050CmpRat(aAltera As Array) As Array

    Local aSaveArea As Array
    Local aCampos	As Array
    Local aFora		As Array

    Default __lF50HEAD  := ExistBlock("F050HEAD")                                

    aSaveArea := GetArea()
    aCampos := {}

    aFora := {"CTJ_FILIAL", "CTJ_RATEIO","CTJ_DESC","CTJ_MOEDLC","CTJ_TPSALD","CTJ_SEQUEN", "CTJ_QTDTOT"}

    aHeader := FinLoadSX3("CTJ",;
                {|cField| Ascan(@aFora,Trim(cField)) <= 0},;
                {   {"X3_TITULO",{|cPar| AllTrim(cPar)}},;
                    {"X3_CAMPO",nil},;
                    {"X3_PICTURE",nil},;
                    {"X3_TAMANHO",nil},;
                    {"X3_DECIMAL",nil},;
                    {"X3_VALID",nil},;
                    {"X3_USADO",nil},;
                    {"X3_TIPO",nil},;
                    {"TMP",nil},;
                    {"X3_CONTEXT",nil}})
    nUsado := Len(aHeader)

    aCampos := FinLoadSX3("CTJ",;
                {|cField| Ascan(@aFora,Trim(cField)) <= 0},;
                {   {"X3_CAMPO",nil},;
                    {"X3_TIPO",nil},;
                    {"X3_TAMANHO",nil},;
                    {"X3_DECIMAL",nil}})
    Aadd(aCampos,{"CTJ_FLAG","L",1,0})

    aAltera := FinLoadSX3("CTJ",;
                {|cField| (Ascan(@aFora,Trim(cField)) <= 0) .And. (Alltrim(cField) <> "CTJ_QTDDIS")},;
                {{"X3_CAMPO",{|cPar| AllTrim(cPar)}}})

    //Ponto de Entrada para inclusao de novos campos.
    If __lF50HEAD
        aCampos := 	ExecBlock("F050HEAD",.f.,.f.,{aCampos})
    EndIf

    RestArea(aSaveArea)

Return aCampos

//-------------------------------------------------------------------
/*/{Protheus.doc} F050MovBco
Verifica se um PA gerou movimentacao bancária

@author Marcel Borges Ferreira
@since  05/02/07
/*/
//-------------------------------------------------------------------
Static Function F050MovBco()

    Local aArea := GetArea()
    Local lMovBco

    dbSelectArea("SE5")
    dbSetOrder(7)
    If MsSeek(xFilial("SE5")+SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA))
        lMovBco := .T.
    Else
        lMovBco := .F.
    EndIf

    RestArea(aArea)

Return lMovBco

//-------------------------------------------------------------------
/*/{Protheus.doc} F050SelPR
Markbrowse da Substituição de Provisórios

@oDlg = Objeto onde se encaixara a MarkBrowse
@cOutMoeda = Tratamento aplicado a outras moedas (<>1)
@nValorS = Valor dos titulos selecionados
@nQtdTit = Quantidade de titulos selecionados
@cMarca = Marca (GetMark())
@oValor = Objeto Valor dos titulos selecionados p/ refresh
@oQtdTit = Objeto Quantidade de titulos selecionados p/refresh
@nMoedSubs = Moeda da Substituicao
@oButton = Objeto Painel superior a ser desabilitado

@author Marcelo Celi Marques
@since  19/05/08
/*/
//-------------------------------------------------------------------
Function F050SelPR(oDlg AS Object, cOutMoeda AS Character, nValorS AS Numeric, nQtdTit AS Numeric, cMarca AS Character,; 
                    oValor AS Object, oQtdTit AS Object, nMoedSubs AS Numeric, oButton AS Object, a1stRow AS Array,;
                    a2ndRow AS Array, nRegSel AS Numeric, aSelFil AS Array, aTmpFil AS Array, aChaveLbn AS Array) AS Logical

    Local lInverte 	    AS Logical
    Local lRet          AS Logical
    Local aRestrict	    AS Array
    Local aCampos       AS Array

    DEFAULT oDlg        := NIL
    DEFAULT cOutMoeda   := ""
    DEFAULT nValorS     := 0
    DEFAULT nQtdTit     := 0
    DEFAULT cMarca      := GetMark()
    DEFAULT oValor      := NIL
    DEFAULT oQtdTit     := NIL
    DEFAULT nMoedSubs   := 0
    DEFAULT oButton     := NIL
    DEFAULT a1stRow     := {}
    DEFAULT a2ndRow     := {}
    DEFAULT nRegSel     := 0
    DEFAULT aSelFil     := {cFilAnt}
    DEFAULT aTmpFil     := {}
    DEFAULT aChaveLbn   := {}

    lInverte 	    := .F.
    lRet            := .T.
    aCampos         := {}

    cAlias          := "__SUBS"

	aRestrict := F050Restr()
    cArqNew	  := f050QryA(@cAliasSE2, aCampos, aRestrict, aSelFil, aTmpFil, cOutMoeda, nMoedSubs)

	If cArqNew == "NOACESS"  // Caso o usuario não tenha nenhuma permissão aborta o processo do substituição
		Help(" ",1,"RECNO")
		lRet := .F.

	ElseIf !Empty( cArqNew )
		dbselectarea(cAliasSE2)
		dbGoTop()
	    If (cAliasSE2)->( BOF() ) .and. (cAliasSE2)->( EOF() )
    		Help(" ",1,"RECNO")
		    lRet := .F.
        Endif
	EndIf

    If lRet
        // Mostra a tela de Titulos Provisorios
        nOpcA := 0
        dbSelectArea("__SUBS")
        dbGoTop()
        cSimb := Pad(Getmv("MV_SIMB"+Alltrim(STR(nMoedSubs))),4)+":"

        @ a1stRow[1] + 016,a1stRow[1] + 155 Say oQtdTit VAR nQtdTit Picture "99999" FONT oDlg:oFont PIXEL Of oDlg
        @ a1stRow[1] + 016,a1stRow[1] + 290 Say oValor	VAR nValorS Picture PesqPict("SE2","E2_VALOR",14) FONT oDlg:oFont PIXEL Of oDlg

        oMark := MsSelect():New("__SUBS","E2_OK","!E2_SALDO",aCampos,@lInverte,@cMarca,a2ndRow,,,oDlg)
        oMark:oBrowse:lhasMark := .T.
        oMark:oBrowse:lCanAllmark := .T.
        oMark:oBrowse:bAllMark := { || FA050Inverte(cMarca,oValor,oQtdtit,@nValorS,@nQtdTit,@oMark,nMoedSubs,aChaveLbn,,.T.,@nRegSel) }
        oMark:bMark := {||Fa050Exibe(@nValorS,@nQtdTit,oValor,oQtdTit,nMoedSubs)}
        oMark:bAval	:= {||Fa050bAval(cMarca,oValor,oQtdtit,@nValorS,@nQtdTit,@oMark,nMoedSubs,aChaveLbn,@nRegSel)}

        oMark:oBrowse:SetFocus()

        CursorArrow()

        oButton:Disable()

    Endif

    FwFreeArray(aCampos)

Return lRet


//-------------------------------------------------------------------
/*/{Protheus.doc} FinA050T
Chamada semi-automatica utilizado pelo gestor financeiro

@author Marcelo Celi Marques
@since  26/03/08
/*/
//-------------------------------------------------------------------
Function FinA050T(aParam)
	cRotinaExec := "FINA050"

	ReCreateBrow("SE2",FinWindow)
	FinA050(,,aParam[1])
	ReCreateBrow("SE2",FinWindow, , .T.)
	dbSelectArea("SE2")

	INCLUI := .F.
	ALTERA := .F.
Return .T.

//-------------------------------------------------------------------
/*/{Protheus.doc} F050GrvSE5
Gravacao de registros do SE5 na inclusao C.Pagar

nOpc = Controle de operacao
lDesdobr = Controle de desdobramento
nRecSE2 = Recno do Registro

@author Mauricio Pequim Jr
@since  20/04/09
/*/
//-------------------------------------------------------------------
Function F050GrvSE5(nOpc AS Numeric, lDesdobr AS Logical, nRecSE2 AS Numeric)

    Local nProxRec  AS Numeric
    Local aAreaGrv  AS Array
    Local lRastro   AS Logical
    //Nova estrutura SE5
    Local oModel    AS Object
    Local cLog      AS Character
    Local oSubFK2   AS Object
    Local oSubFK3   AS Object
    Local oSubFK4   AS Object
    Local oSubFK5   AS Object
    Local oSubFK6   AS Object
    Local oSubFKA   AS Object
    Local aAreaAnt  AS Array
    Local cOrig     AS Character
    Local cCamposE5 AS Character
    Local cChaveSE2 AS Character

    DEFAULT nOpc       := 0
    DEFAULT lDesdobr   := .F.
    DEFAULT nRecSE2    := 0
    DEFAULT __lNRasDSD := SuperGetMV("MV_NRASDSD",.T.,.F.)
    
    nProxRec := 0
    aAreaGrv := GetArea()
    lRastro := FVerRstFin()

    oModel    := NIL
    cLog      := ""
    oSubFK2   := NIL
    oSubFK3   := NIL
    oSubFK4   := NIL
    oSubFK5   := NIL
    oSubFK6   := NIL
    oSubFKA   := NIL
    aAreaAnt  := {}
    cOrig     := Funname()
    cCamposE5 := ""
    cChaveSE2 := SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)
                                                            //da maneira anterior ao implementado com o rastreamento.
    If nOpc == 1  //Inclusao

        If (lDesdobr .AND. lRastro .AND. !__lNRasDSD)

            //Em caso de rastreamento, posiciono no registro do titulo gerador do desdobramento
            SE2->(dbGoto(nRecSE2))

            aAreaAnt := GetArea()
            oModel :=  FWLoadModel('FINM020')//Bx Contas a Pagar
            oModel:SetOperation(3) // Inclusao
            oModel:Activate()
            oModel:SetValue( "MASTER", "E5_GRV", .T. )
            oModel:SetValue( "MASTER", "NOVOPROC", .T. )
            oSubFK2  := oModel:GetModel("FK2DETAIL")
            oSubFK3  := oModel:GetModel("FK3DETAIL")
            oSubFK4  := oModel:GetModel("FK4DETAIL")
            oSubFK5  := oModel:GetModel("FK5DETAIL")
            oSubFK6  := oModel:GetModel("FK6DETAIL")
            oSubFKA  := oModel:GetModel("FKADETAIL")

            cChaveTit := xFilial("SE2") + "|" + SE2->E2_PREFIXO + "|" + SE2->E2_NUM     + "|" + SE2->E2_PARCELA + "|" + ;
            SE2->E2_TIPO    + "|" + SE2->E2_FORNECE + "|" + SE2->E2_LOJA
            cChaveFK7 := FINGRVFK7("SE2", cChaveTit)
            cIdDoc	:= FWUUIDV4()
            cCamposE5:="{"

            cCamposE5 += "{'E5_TIPO'    , '"+SE2->E2_TIPO+"'}"
            cCamposE5 += ",{'E5_PREFIXO' , '"+SE2->E2_PREFIXO+"'}"
            cCamposE5 += ",{'E5_NUMERO'  , '"+SE2->E2_NUM+"'}"
            cCamposE5 += ",{'E5_PARCELA' , '"+SE2->E2_PARCELA+"'}"
            cCamposE5 += ",{'E5_CLIFOR'  , '"+SE2->E2_FORNECE+"'}"
            cCamposE5 += ",{'E5_FORNECE' , '"+SE2->E2_FORNECE+"'}"
            cCamposE5 += ",{'E5_LOJA'    , '"+SE2->E2_LOJA+"'}"
            cCamposE5+= ",{'E5_DTDIGIT', 	dDataBase  }"
            cCamposE5 += ",{'E5_BENEF'   , '"+SE2->E2_NOMFOR+"' }"
            cCamposE5 += ",{'E5_DTDISPO' , CTOD('"+DTOC(SE5->E5_DATA)+"') } "

            // Grava ID do titulo
            If !oSubFKA:IsEmpty()
                oSubFKA:AddLine()
            EndIf

            oSubFKA:SetValue( "FKA_IDORIG", cIdDoc )
            oSubFKA:SetValue( "FKA_TABORI", "FK2" )
            oSubFK2:SetValue( "FK2_IDDOC"  , cChaveFK7 )
            oSubFK2:SetValue( "FK2_DATA", SE2->E2_EMISSAO )
            oSubFK2:SetValue( "FK2_NATURE",  SE2->E2_NATUREZ )
            oSubFK2:SetValue( "FK2_LA", "S")
            oSubFK2:SetValue( "FK2_MOTBX", "DSD" )
            oSubFK2:SetValue( "FK2_RECPAG","P" )
            oSubFK2:SetValue( "FK2_HISTOR",  Rtrim(SubStr(SE2->E2_HIST,1,TAMSX3("FK2_HISTOR")[1])))
            oSubFK2:SetValue( "FK2_FILORI", SE2->E2_FILORIG )
            oSubFK2:SetValue( "FK2_ORIGEM", cOrig )
            oSubFK2:SetValue( "FK2_TPDOC", "BA" )

            If cPaisLoc <> "BRA"
                SA6->(DbSetOrder(1))
                SA6->(DbSeek(xFilial()+cBancoAdt+cAgenciaAdt+cNumCon))
                If( Max(IIf(Type("SA6->A6_MOEDAP")=="U",SA6->A6_MOEDA,SA6->A6_MOEDAP),1) )== 1

                    oSubFK2:SetValue( "FK2_VALOR", SE2->E2_VLCRUZ)
                    oSubFK2:SetValue( "FK2_VLMOE2", SE2->E2_VALOR)
                    oSubFK2:SetValue( "FK2_MOEDA", Strzero(Max(IIf(Type('SA6->A6_MOEDAP')=='U',SA6->A6_MOEDA,SA6->A6_MOEDAP),1),2) )
                Else

                    oSubFK2:SetValue( "FK2_VALOR", SE2->E2_VALOR)
                    oSubFK2:SetValue( "FK2_VLMOE2", SE2->E2_VLCRUZ  )
                    oSubFK2:SetValue( "FK2_MOEDA", Strzero(Max(IIf(Type('SA6->A6_MOEDAP')=='U',SA6->A6_MOEDA,SA6->A6_MOEDAP),1),2) )
                Endif
            Else
                oSubFK2:SetValue( "FK2_MOEDA", "01" )
                oSubFK2:SetValue( "FK2_VALOR", SE2->E2_VLCRUZ )
                oSubFK2:SetValue( "FK2_VLMOE2",  SE2->E2_VALOR  )
            Endif

            oSubFK2:SetValue( "FK2_TXMOED", SE2->E2_TXMOEDA)

            cCamposE5+="}"

            oModel:SetValue( "MASTER", "E5_CAMPOS", cCamposE5 )
            If oModel:VldData()
                oModel:CommitData()
                oModel:DeActivate()
            Else
                cLog := cValToChar(oModel:GetErrorMessage()[4]) + ' - '
                cLog += cValToChar(oModel:GetErrorMessage()[5]) + ' - '
                cLog += cValToChar(oModel:GetErrorMessage()[6])

                If (Type("lF050Auto") == "L" .and. !lF050Auto)
                    Help( ,,"M050VALID",,cLog, 1, 0 )
                EndIf
            EndIf

            RestArea(aAreaAnt )
            Reclock("SE2", .F. )
            nSE2Rec := Recno()
            SE2->E2_BAIXA		:= dDatabase
            SE2->E2_MOVIMEN	:= dDatabase
            SE2->E2_DESCONT	:= SE2->E2_SDDECRE
            SE2->E2_JUROS		:= SE2->E2_SDACRES
            SE2->E2_VALLIQ		:= SE2->(E2_VLCRUZ+E2_SDACRES-E2_SDDECRE)
            SE2->E2_SALDO		:= 0
            SE2->E2_SDACRES	:= 0
            SE2->E2_SDDECRE	:= 0
            SE2->E2_STATUS		:= "D"
            MsUnlock()
        EndIf

    ElseIf nOpc == 2  //Exclusao de titulo

        //Limpa chaves de relacionamento (SE5 e SEF)
        SE5->(dbSetOrder(7))
        If SE5->(dbSeek(xFilial("SE5")+ cChaveSE2))
            While !SE5->(Eof()) .and. xFilial("SE5") == SE5->E5_FILIAL .and. SE5->(E5_PREFIXO+E5_NUMERO+E5_PARCELA+E5_TIPO+E5_FORNECE+E5_LOJA) == cChaveSE2
                
                If SE2->(!EOF() .and. !Empty(E2_FILORIG))
                    If SE5->E5_FILORIG <> SE2->E2_FILORIG
                        SE5->(dbSkip())
                        Loop
                    EndIf
                EndIf                
                
                nAtuRec := SE5->(RECNO())
                SE5->(DbSkip())
                nProxRec := SE5->(Recno())
                SE5->(dbGoto(nAtuRec))
                dbSelectArea( "FK2" )//limpando dados de estorno
                FK2->( DbSetOrder( 1 ) )//FK2_FILIAL+FK2_IDMOV
                If SE5->E5_TABORI== "FK2" .AND. MsSeek( xFilial("FK2") + SE5->E5_IDORIG )
                    
                    //Posiciona no processo FKA para o Model não se perder na função FINProcFKs
                    FKA->( DbSetOrder( 3 ) )//FKA_FILIAL+FKA_TABORI+FKA_IDORIG
                    FKA->( MsSeek( xFilial("FKA") + 'FK2' + FK2->FK2_IDFK2 ) )
                    
                    cCamposE5:="{"
                    cCamposE5+="{'E5_KEY',E5_PREFIXO+E5_NUMERO+E5_PARCELA+E5_TIPO+E5_CLIFOR+E5_LOJA } "
                    cCamposE5+= ",{'E5_PREFIXO', '' }"
                    cCamposE5+= ",{'E5_NUMERO', '' }"
                    cCamposE5+= ",{'E5_PARCELA', '' }"
                    cCamposE5+= ",{'E5_TIPO', '' }"
                    cCamposE5+= ",{'E5_LA', 'S' }"
                    cCamposE5+="}"

                    aAreaAnt := GetArea()

                    oModel :=  FWLoadModel('FINM020')//Baixas a Pagar
                    oModel:SetOperation( 4 ) //Alteração
                    oModel:Activate()
                    oModel:SetValue( "MASTER", "E5_GRV", .T. ) //habilita gravação de SE5
                    oModel:SetValue( "MASTER", "E5_CAMPOS", cCamposE5 ) //Informa os campos da SE5 que serão gravados indepentes de FK5

                    oSubFK2  := oModel:GetModel("FK2DETAIL")
                    oSubFKA  := oModel:GetModel("FKADETAIL")
                    oSubFKA:SeekLine( { {"FKA_IDORIG", SE5->E5_IDORIG } } )

                    //Dados do movimento
                    oSubFK2 := oModel:GetModel( "FK2DETAIL" )
                    oSubFK2 :SetValue( "FK2_LA", "S" )

                    If oModel:VldData()
                        oModel:CommitData()
                        oModel:DeActivate()
                    Else
                        cLog := cValToChar(oModel:GetErrorMessage()[4]) + ' - '
                        cLog += cValToChar(oModel:GetErrorMessage()[5]) + ' - '
                        cLog += cValToChar(oModel:GetErrorMessage()[6])

                        Help( ,,"M050VALID",,cLog, 1, 0 )
                    EndIf

                    RestArea(aAreaAnt)
                EndIf
                FKCOMMIT()
                SE5->(dbGoto(nProxRec))
            Enddo
        EndIf
    EndIf

    RestArea(aAreaGrv)

    FWFreeArray(aAreaGrv)
    FWFreeArray(aAreaAnt)

Return NIL

//-------------------------------------------------------------------
/*/{Protheus.doc} FA050VLSUB
Valida a Tela da Substituicao de provisorios

@author Eduardo Ramalho
@since  11/26/09
/*/
//-------------------------------------------------------------------
Static Function FA050VLSUB()

    Local lRet := .T.
    Local lFA050VLS:= ExistBlock("FA050VLS")

    If nQtdTit < 1
        lRet := .F.
        Aviso(STR0115, STR0151, {"Ok"}) //"Atencao"##"Selecionar o título a ser substituido."
    EndIf

    If lFA050VLS .And. lRet
        lRet := ExecBlock("FA050VLS",.F.,.F.)
    EndIf

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} F050VerAlt
Verifica a possibilidade de Alteração de um titulo que teve
seus impostos(PCC) Retido em outro Titulo(Retentor)

@author Totvs SA
@since  05/11/09
/*/
//-------------------------------------------------------------------
Function F050VerAlt(lHelp)

    Local aArea 	 := GetArea()
    Local aAreaSE2 	 := SE2->(GetArea())
    Local aAreaSFQ 	 := SFQ->(GetArea())
    Local lRet		 := .T.
    Local lPCCBaixa  := SuperGetMv("MV_BX10925",.T.,"2") == "1"  //Controla o Pis Cofins e Csll na baixa
    Local lIRPFBaixa := IIf( cPaisLoc == "BRA", SA2->A2_CALCIRF == "2", .F.)

    DEFAULT lHelp    := .T.

    //Caso controle retencao de PCC e o titulo possua calculo de PCC e a retencao seja na emissao
    If !lPccBaixa .and. SE2->(E2_PIS + E2_COFINS + E2_CSLL) > 0
        If SE2->E2_PRETPIS == "2" .or. SE2->E2_PRETCOF == "2" .or. SE2->E2_PRETCSL == "2"
            SFQ->(DbSetOrder(2)) //-- FQ_FILIAL+FQ_ENTDES+FQ_PREFDES+FQ_NUMDES+FQ_PARCDES+FQ_TIPODES+FQ_CFDES+FQ_LOJADES

            If SFQ->(DbSeek(xFilial("SFQ")+"SE2"+SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)))
                //-- Localiza Tit Retentor
                SE2->(DbSetOrder(1)) //-- E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA

                If SE2->(DbSeek(xFilial("SE2")+SFQ->(FQ_PREFORI+FQ_NUMORI+FQ_PARCORI+FQ_TIPOORI+FQ_CFORI+FQ_LOJAORI)))

                    //-- Verifica se o titulo e seus impostos estão Baixados
                    If SE2->E2_VALOR == SE2->E2_SALDO
                        If F050BxImp()

                            If lHelp
                                Help(" ",1,"F050BXPCC")
                            EndIf

                            lRet := .F.
                        EndIf
                    Else
                        lRet := .F.
                    EndIf
                Else
                    lRet := .F.
                EndIf
            EndIf
        Else
            If F050BxImp()
                If lHelp
                    Help(" ",1,"F050BXPCC")
                Endif
                lRet := .F.
            EndIf
        EndIf
    EndIf

    If !lIRPFBaixa .and. SE2->E2_IRRF > 0
        If F050BxImp()
            If lHelp
                Help(" ",1,"F050BXPCC")
            Endif
            lRet := .F.
        EndIf
    Endif

    RestArea(aAreaSE2)
    RestArea(aAreaSFQ)
    RestArea(aArea)

    FWFreeArray(aAreaSE2)
    FWFreeArray(aAreaSFQ)
    FWFreeArray(aArea)

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} F050BxPai
Verifica se o titulo Pai relacionado ao titulo filho foi baixado

@author Totvs SA
@since  05/11/09
/*/
//-------------------------------------------------------------------
Function F050BxPai()

    Local lRet := .F.
    Local aAreaSE2 := SE2->(GetArea())
    Local aArea := GetArea()

    Private nRecPai := 0 // Recno do Título PAI

    If !Empty(SE2->E2_TITPAI)

        //Busco o Titulo Pai
        If Fa050Pai()
            SE2->(DbGoTo(nRecPai))
            If SE2->E2_VALOR !=	SE2->E2_SALDO
                lRet := .T.
            EndIf
        EndIf

    Endif

    RestArea(aAreaSE2)
    RestArea(aArea)

    FWFreeArray(aAreaSE2)
    FWFreeArray(aArea)

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} FRetOTits
Verifica se o título ATUAL que está sendo ALTERADO, reteve
impostos referentes a outro título em sua INCLUSÃO

@author Adrianne Furtado
@since 23/12/09
@version 12
@return nValInss, Valor do INSS calculado somado ao valor retido em outros títulos
@param nValInss, numeric, Valor calculado do INSS
@type function
/*/
//-------------------------------------------------------------------
Function FRetOTits( nValInss )

    Local aAreaSFQ := {}
    Local aAreaSE2 := {}
    Local nDifer := 0 //Valor que esse titulo reteve referente a outro(s) titulo(s)a
    Local cChaveSFQ := ""

    //SOMENTE EFETUARÁ O CALCULO SE FOR ALTERAÇÃO DO TITULO
    If FwIsInCallStack("AxAltera") .And. Funname() == "FINA050"
        aAreaSFQ := SFQ->( GetArea() )
        SFQ->( dbSetOrder(1) ) //FQ_FILIAL+FQ_ENTORI+FQ_PREFORI+FQ_NUMORI+FQ_PARCORI+FQ_TIPOORI+FQ_CFORI+FQ_LOJAORI

        cChaveSFQ := "SE2" + SE2->( E2_PREFIXO + E2_NUM + E2_PARCELA + E2_TIPO + E2_FORNECE + E2_LOJA )

        If SFQ->( msSeek( FWxFilial("SFQ") + cChaveSFQ ) )
	        aAreaSE2 := SE2->( GetArea() )
	        SE2->( dbSetOrder(1) ) //E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA

	        While SFQ->( FQ_ENTORI + FQ_PREFORI + FQ_NUMORI + FQ_PARCORI + FQ_TIPOORI + FQ_CFORI + FQ_LOJAORI ) == cChaveSFQ
	            If AllTrim(SFQ->FQ_TPIMP) == "INS"
	                If SE2->( msSeek( FWxFilial("SE2") + SFQ->( FQ_PREFDES + FQ_NUMDES + FQ_PARCDES + FQ_TIPODES + FQ_CFDES + FQ_LOJADES ) ) )
	                	nDifer += SE2->E2_VRETINS
	                EndIf
	            EndIf

	            SFQ->( dbSkip() )
	        EndDo

	        nValInss += nDifer

	        RestArea( aAreaSE2 )
	        FwFreeArray( aAreaSE2 )
        EndIf

        RestArea( aAreaSFQ )
        FwFreeArray( aAreaSFQ )
    EndIf

Return nValInss

//-------------------------------------------------------------------
/*/{Protheus.doc} F050BSIMP
Verificacao do uso de base diferenciada para impostos

@author Adrianne Furtado
@since  12/04/10
/*/
//-------------------------------------------------------------------
Function F050BSIMP(nOpcao,nImposto)

    Local lRet 		:= .F.
    Local lSE2Ok 	:= .F.
    Local aArea		:= GetArea()
    Local lCondImp	:= .T.
    Local lSimples	:= __lLocBRA .and. SA2->A2_CALCIRF == "3"
    Local lCposImp  := __lLocBRA

    lIrProg := IIf(__lLocBRA,IIf(!Empty(SA2->A2_IRPROG),SA2->A2_IRPROG,"2"),"2")

    DEFAULT nOpcao := 1  // 1 = Vericar campos e calculo dos impostos; 2 = Verificar apenas existencia dos campos
    DEFAULT nImposto := 0  // 0 = Vericacao Geral

    If nOpcao == 1
        //Se existirem os campos de base de impostos
        //Verifica se o cliente e a natureza calcula impostos
        lCondImp := __lLocBRA .and. SED->(MsSeek(xFilial("SED")+M->E2_NATUREZ))
        lSe2Ok := !Empty(M->E2_NATUREZ) .and. !EMPTY(M->E2_FORNECE)
    ElseIf nOpcao == 2
        //Se existirem os campos de base de impostos
        lRet := __lLocBRA
        lSe2Ok := .F.
    ElseIf nOpcao == 3
        //Verifica apenas se calcula algum dos impostos (Desdobramento)
        lCposImp := .T.
        lSe2Ok := .T.
    Endif

    If __lLocBRA .and. lSe2Ok .and. lCondImp

        Do Case

            Case nImposto == 1		//Irrf
                If SED->ED_CALCIRF == "S" .And. !lSimples
                    //IRRF Pessoa Fisica na Baixa
                    If	(SA2->A2_TIPO == 'F' .OR. (SA2->A2_TIPO == "J" .AND. lIrProg == "1"))
                        lRet := F050BIRPF(nOpcao)
                    Else
                        //IRRF Juridica
                        lRet := .T.
                    Endif
                Endif

            Case nImposto == 2		//PIS
                If (SED->ED_CALCPIS == "S" .and. SA2->A2_RECPIS $ "2") .or. (SED->ED_PCAPPIS > 0) //ESTE ITEM É PROVISORIO PARA O SPED PIS COF
                    lRet := .T.
                Endif

            Case nImposto == 3		//COFINS
                If (SED->ED_CALCCOF == "S" .and. SA2->A2_RECCOFI $ "2") .or. (SED->ED_PCAPCOF > 0) //ESTE ITEM É PROVISORIO PARA O SPED PIS COF
                    lRet := .T.
                Endif

            Case nImposto == 4		//CSLL
                If	(SED->ED_CALCCSL == "S" .and. SA2->A2_RECCSLL $ "2")
                    lRet := .T.
                Endif

            Case nImposto == 5		//INSS
                If	(SED->ED_CALCINS == "S" .And. SA2->A2_RECINSS == "S")
                    lRet := .T.
                Endif

            Case nImposto == 6		//ISS
                If (SED->ED_CALCISS == "S" .And. SA2->A2_RECISS == "N")
                    lRet := .T.
                Endif

            //Verifica se algum imposto eh calculado
            //Utilizado para verificar se o desdobramento sera feito por rotina automatica e calcular impostos
            //Ou utilizar a rotina padrao
            Case nImposto == 7
                //IRRF
                If SED->ED_CALCIRF == "S" .And. !lSimples
                    //IRRF Pessoa Fisica na Baixa
                    If	(SA2->A2_TIPO == 'F' .OR. (SA2->A2_TIPO == "J" .AND. lIrProg == "1")) .AND. SA2->A2_CALCIRF == '2'
                        lRet := F050BIRPF(nOpcao)
                    Else
                        //IRRF Juridica
                        lRet := .T.
                    Endif
                Endif

                //COFINS
                If !lRet .and. (SED->ED_CALCPIS == "S" .and. SA2->A2_RECPIS $ "2")
                    lRet := .T.
                Endif

                //COFINS
                If !lRet .and. (SED->ED_CALCCOF == "S" .and. SA2->A2_RECCOFI $ "2")
                    lRet := .T.
                Endif

                //CSLL
                If	!lRet .and. (SED->ED_CALCCSL == "S" .and. SA2->A2_RECCSLL $ "2")
                    lRet := .T.
                Endif

                //INSS
                If	!lRet .and. (SED->ED_CALCINS == "S" .And. SA2->A2_RECINSS == "S")
                    lRet := .T.
                Endif

                //ISS
                If !lRet .and. (SED->ED_CALCISS == "S" .And. SA2->A2_RECISS == "N")
                    lRet := .T.
                Endif
        END CASE

    Endif

    RestArea(aArea)
    FWFreeArray(aArea)

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} F050ImpAut
Verifica se os impostos foram informados no array da rotina
automatica ou se devem ser calculados normalmente.

@author Mauricio Pequim Jr
@since  12/04/10
/*/
//-------------------------------------------------------------------
Function F050ImpAut(cImposto, nPsimp)

    Local nT As Numeric
    Local lRet As Logical

    DEFAULT cImposto := ""
    DEFAULT nPsimp	:= 0

    aAutoCab := If(Type("aAutoCab") != "A",{},aAutoCab)
    nT := 0
    lRet := .F.

    If Len(aAutoCab) > 0

        //Verifico se algum imposto foi enviado no array aRotAuto
        //Significa que o imposto foi preh calculado e não deve ser calculado novamente
        IF !Empty(cImposto) .and. (nT := ascan(aAutoCab,{|x| Alltrim(x[1]) == cImposto .and. x[2] > 0}) ) > 0
            lRet    := .T.
            nPsimp	:= nT
        Endif

    Endif

Return (lRet)

//-------------------------------------------------------------------
/*/{Protheus.doc} F050BIRPF
Verificacao do uso do campo E2_BASEIRF - IRPF BAIXA

@author Mauricio Pequim Jr
@since  20/05/09
/*/
//-------------------------------------------------------------------
Function F050BIRPF(nOpcao)

    Local lRet := .F.
    Local lSE2Ok := .F.
    Local aArea	:= GetArea()
    Local cChaveSA2 := ""

    DEFAULT nOpcao := 1  // 1 = Inclusão, Alteracao. 2 = Baixa 3= Bordero, manutencao de bordero

    lIrProg := IIf(__lLocBRA, IIf(!Empty(SA2->A2_IRPROG),SA2->A2_IRPROG,"2"),"2")

    If nOpcao == 1 .OR. nOpcao == 2
        dbSelectArea("SA2")
        SA2->(dbSetOrder(1))
        If nOpcao == 1
            cChaveSA2 := xFilial("SA2") + M->E2_FORNECE + SPACE(TamSx3("E2_FORNECE")[1] - LEN(M->E2_FORNECE))+;
                            M->E2_LOJA + SPACE(TamSx3("E2_LOJA")[1] - LEN(M->E2_LOJA))
            SA2->(dbSeek(cChaveSA2))
        Else
            SA2->(dbSeek(xFilial("SA2") + SE2->(E2_FORNECE + E2_LOJA)))
        Endif
        If !SA2->(Eof())
            lIrProg := IIf(__lLocBRA, IIf(!Empty(SA2->A2_IRPROG),SA2->A2_IRPROG,"2"),"2")
        Endif
    Endif

    If nOpcao == 1
        lSe2Ok := !Empty(M->E2_NATUREZ) .and. !EMPTY(M->E2_FORNECE)
        If __lLocBRA .and. lSe2Ok .and. ( nModulo == 6 .Or. lF050Auto) .AND. ;
            SED->(MsSeek(xFilial("SED")+M->E2_NATUREZ)) .AND. ;
            SED->ED_CALCIRF == 'S' .AND. ;
            ((SA2->A2_TIPO == 'F' .OR. (SA2->A2_TIPO == "J" .AND. lIrProg == "1")) .AND. SA2->A2_CALCIRF == '2')
            lRet := .T.
        Endif
    ElseIf nOpcao == 2
        lSe2Ok := !Empty(SE2->E2_NATUREZ) .and. !EMPTY(SE2->E2_FORNECE)
        If __lLocBRA .and. lSe2Ok .and. nModulo == 6
            lRet := .T.
        Endif
    ElseIf nOpcao == 3
        If __lLocBRA .and. nModulo == 6 .AND. SED->ED_CALCIRF == 'S' .AND. ;
            ((SA2->A2_TIPO == 'F' .OR. (SA2->A2_TIPO == "J" .AND. lIrProg == "1")) .AND. SA2->A2_CALCIRF == '2')
            lRet := .T.
        Endif
    Endif

    RestArea(aArea)
    FWFreeArray(aArea)

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} Fa050Agend
Altera a Data de Agendamento do Título.

@author José Lucas
@since  11/02/10
/*/
//-------------------------------------------------------------------
Function Fa050Agend()

    LOCAL aArea      as Array
    LOCAL cSequencia as Character
    LOCAL cTitulo    as Character
    LOCAL dDataAgend as Date
    LOCAL lGravaLog  as Logical
    Local lRet       as Logical
    LOCAL nOpca      as Numeric

    PRIVATE aSE2FI2	 As Array 
    PRIVATE _Opc     As Numeric 

    Default __lMetric  := FwLibVersion() >= "20210517"

    aArea      := GetArea()
    dDataAgend := CTOD("")
    nOpca      := 0
    cTitulo	   := STR0170		//"Alteração no agendamento."
    cSequencia := "000000"
    lGravaLog  := GetNewpar( "MV_CTBLGET" , .F. )
    lRet	   := .T.

    aSE2FI2	   := {}
    _Opc       := 4    

    // Validação Siafi
    If FinTemDH()
        lRet := .F.
    Endif

    If __lMetric
        // Metrica de controle de acessos 
        FwCustomMetrics():setSumMetric(Alltrim(ProcName()), "financeiro-protheus_qtd-por-acesso_total", 1)
    Endif

    If lRet

        dDataAgend := IIF(EMPTY(SE2->E2_DATAAGE),SE2->E2_VENCREA,SE2->E2_DATAAGE)

        DbSelectArea("FI2")

        DEFINE MSDIALOG oDlg TITLE cTitulo FROM 00,00 TO 150,500 OF oMainWnd PIXEL

        @ 035, 035 SAY STR0171 			SIZE 090,10 OF oDlg PIXEL
        @ 035, 124 MSGET oDataAge  	VAR dDataAgend		SIZE 090,10 PICTURE "@D" VALID Fa050VAge(dDataAgend)	OF oDlg PIXEL

        ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar(oDlg,{||nOpca:=1,if(Fa050AOk(oDlg),nOpca:=1,nOpca := 0)},{||oDlg:End()}) CENTERED

        If nOpcA == 1
            // Grava a Data de Agendamento.
            If lGravaLog
                cSequencia := Fa050GetSq(SE2->E2_NUMBOR,SE2->E2_PREFIXO,SE2->E2_NUM,SE2->E2_PARCELA,SE2->E2_TIPO,SE2->E2_FORNECE,SE2->E2_LOJA)
                AADD(aSE2FI2,{cSequencia,"ALTERAÇÃO NA DATA DE AGENDAMENTO",Trans(SE2->E2_DATAAGE,"@D 99/99/99"),Trans(dDataAgend,"@D 99/99/99"),"E2_DATAAGE","D"})
            EndIf
            RecLock("SE2",.F.)
            SE2->E2_DATAAGE := dDataAgend
            MsUnLock()

            If Len(aSE2FI2) > 0 .and. lGravaLog
                // Efetua a gravacao do Histórico de Agendamentos.
                F050GrvFI2(lGravaLog)
            EndIf
        EndIf
    Endif

    RestArea( aArea )
    FWFreeArray(aArea)

Return

//-------------------------------------------------------------------
/*/{Protheus.doc} Fa050VAge
Validar a digitação da Data de Agendamento.

@author José Lucas
@since  11/02/10
/*/
//-------------------------------------------------------------------
Function Fa050VAge(dDataAgend)

    LOCAL lRet 		:= .T.

    If dDataAgend < SE2->E2_EMISSAO
        MsgAlert("Data de agendamento dever ser maior ou igual a data da Emissão.","Atenção!")
        lRet := .F.
    EndIf

Return( lRet )

//-------------------------------------------------------------------
/*/{Protheus.doc} Fa050AOk
Validar e fechar Dialogo da caixa de edição da data de Agendamento.

@author José Lucas
@since  11/02/10
/*/
//-------------------------------------------------------------------
Function Fa050AOk(oDlg)
    LOCAL lRet := .T.
    oDlg:End()
Return( lRet )

//-------------------------------------------------------------------
/*/{Protheus.doc} Fa050GetSq
Retornar o próximo numero sequencial para gravação do Log
com base na existencia do Título.

@author José Lucas
@since  11/02/10
/*/
//-------------------------------------------------------------------
Function Fa050GetSq(cNumBor,cPrefixo,cTitulo,cParcela,cTipo,cFornece,cLoja)
    Local aArea			:= GetArea()
    Local cQuery		:= ""
    Local cSequencia	:= "000000"

    If Select("QRYFI2") > 0
        QRYFI2->(DbCloseArea())
    EndIf

    If TcGetDb() $ "INFORMIX*ORACLE"
        cQuery := "SELECT NVL(MAX(FI2_SEQ),'0') MAXSEQ FROM "
    ElseIf  TcGetDb() $ "DB2*POSTGRES"  .OR. ( TcGetDb() == "DB2/400" .And. Upper(TcSrvType()) == "ISERIES" )
        cQuery := "SELECT COALESCE(MAX(FI2_SEQ),'0') MAXSEQ FROM "
    Else
        cQuery := "SELECT ISNULL(MAX(FI2_SEQ),'0') MAXSEQ FROM "
    EndIf

    cQuery += RetSqlName("FI2") + " FI2 "
    cQuery += " WHERE"
    cQuery += " FI2_FILIAL = '" + xFilial("FI2") + "' "
    cQuery += " AND FI2_CARTEI = '2' "
    cQuery += " AND FI2_NUMBOR = '" + cNumBor + "' "
    cQuery += " AND FI2_PREFIX = '" + cPrefixo + "' "
    cQuery += " AND FI2_TITULO = '" + cTitulo + "' "
    cQuery += " AND FI2_PARCEL = '" + cParcela + "' "
    cQuery += " AND FI2_TIPO = '" + cTipo + "' "
    cQuery += " AND FI2_CODFOR = '" + cFornece + "' "
    cQuery += " AND FI2_LOJFOR = '" + cLoja + "' "
    cQuery += " AND FI2_GERADO = '2' "
    cQuery += " AND D_E_L_E_T_ = ' ' "

    cQuery := ChangeQuery(cQuery)

    dbUseArea( .T., "TOPCONN", TCGenQry(,,cQuery), 'QRYFI2', .F., .T.)

    QRYFI2->(dbGoTop())
    If QRYFI2->(!Eof())
        cSequencia := PadL(AllTrim(QRYFI2->MAXSEQ), TamSX3("FI2_SEQ")[1], "0")
    EndIf
    QRYFI2->(DbCloseArea())

    cSequencia := Soma1(cSequencia)

    RestArea(aArea)

Return(cSequencia)

//-------------------------------------------------------------------
/*/{Protheus.doc} F050VlAdFoLj
Valida fornecedor e loja para titulo de adiantamento de
pedido de compra ou documento de entrada.

@author Totvs SA
@since  20/05/10
/*/
//-------------------------------------------------------------------
Function F050VlAdFoLj()

    Local lOk := .T.

    If FunName() = "MATA121"
        If Type("cA120Forn") != "U" .and. Type("cA120Loj") != "U"
            If M->E2_FORNECE+M->E2_LOJA != cA120Forn+cA120Loj
                lOk := .F.
            Endif
        Endif
    Elseif FunName() = "MATA103"
        If Type("cA100For") != "U" .and. Type("cLoja") != "U"
            If M->E2_FORNECE+M->E2_LOJA != cA100For+cLoja
                lOk := .F.
            Endif
        Endif
    Endif

    If !lOk
        Aviso(STR0115,STR0166,{ "Ok" }) //"ATENCAO"#"Por tratar-se de título para processo de adiantamento, é obrigatório que o código do fornecedor e loja sejam os mesmos do 'Pedido de Compra/Documento de Entrada'."
    Endif

Return lOk


//-------------------------------------------------------------------
/*/{Protheus.doc} Fa50Vendor
Função que validará se o título foi gerado por uma baixa
automatica do tipo VENDOR. Caso positivo, podera ser
deletado, igual ao processo da baixa manual por VENDOR.

@author Clovis Magenta
@since  18/08/10
/*/
//-------------------------------------------------------------------
Function Fa50Vendor()

    Local lDelete 	 := .F.
    Local lBxAutVen := SuperGetMv("MV_BXAUTVE",.T.,.F.)
    Local aArea := GetArea()

    If lBxAutVen .and. Alltrim(SE2->E2_ORIGEM) == "FINA090" .and. Alltrim(SE2->E2_NATUREZ) == "VENDOR"
        lDelete := .T.
    Endif

    RestArea(aArea)

Return lDelete

//-------------------------------------------------------------------
/*/{Protheus.doc} isFunrural
Função que verifica se o fornecedor é FUNRURAL e se é considerado
para o calculo diferenciado para MP447 - INSS

@author Clovis Magenta
@since  02/07/10
/*/
//-------------------------------------------------------------------
STATIC Function isFunrural()

    Local lRural := .F.
    Local aAreaSE2 := SA2->(GetArea())
    Local cOrigem  := FunName()

    dbSelectArea("SA2")
    dbSetOrder(1)
    If Alltrim(cOrigem) == "FINA050" .OR. Type("M->E2_FORNECE")<>"U"
        DbSeek(xFilial("SA2")+M->E2_FORNECE+M->E2_LOJA)
    Elseif Alltrim(cOrigem) == "MATA103"
        DbSeek(xFilial("SA2")+SF1->(F1_FORNECE+F1_LOJA))
    Endif
    If Alltrim(SA2->A2_TIPORUR)$"L|F"
        lRural := .T.
    Endif

    RestArea(aAreaSE2)

Return lRural


//-------------------------------------------------------------------
/*/{Protheus.doc} FA050MCpos
Permitir colocar em memoria o conteudo de campos ao
selecionar a opcao "Visualizar" do contas a pagar

@author Gustavo Henrique
@since  22/11/10
/*/
//-------------------------------------------------------------------
Function FA050MCpos()

    //Controla o Pis Cofins e Csll na baixa
    Local lPCCBaixa := SuperGetMv("MV_BX10925",.T.,"2") == "1"

    // Atribuo o valor que realmente foi retido nos campos do PCC
    // para ser apresentado na tela do AxVisual e nao afetar os
    // titulos de PCC gerados na emissão.
    If __lLocBRA .and. !lPccBaixa
        M->E2_PIS    := Iif( Empty(SE2->E2_VRETPIS), SE2->E2_PIS   , SE2->E2_VRETPIS )
        M->E2_COFINS := Iif( Empty(SE2->E2_VRETCOF), SE2->E2_COFINS, SE2->E2_VRETCOF )
        M->E2_CSLL   := Iif( Empty(SE2->E2_VRETCSL), SE2->E2_CSLL  , SE2->E2_VRETCSL )
    EndIf

Return .T.

//-------------------------------------------------------------------
/*/{Protheus.doc} Fa050Docs
Efetua manutencao nos documentos

@author Andre Lago
@since  21/07/10
/*/
//-------------------------------------------------------------------
Function Fa050Docs()

    LOCAL aArea      	As Array
    Local aHead 		As Array
    Local aCol	 		As Array
    Local aAlter    	As Array
    Local aSizeTela		As Array
    Local aObjects 		As Array
    Local aInfo         As Array
    Local aPosObj       As Array
    LOCAL cTitulo	 	As Character
    Local cCampos 		As Character
    Local cLinOk		As Character
    Local cFieldOk   	As Character
    Local cSuperDel   	As Character
    Local cDelOk      	As Character
    Local lIgual		As Logical 
    Local lAchou		As Logical 
    Local lF050DOCS		As Logical 
    LOCAL nOpca      	As Numeric 
    Local nX			As Numeric 
    Local nCols    		As Numeric 
    Local nFreeze    	As Numeric 
    Local nMax       	As Numeric 
    Local nPosDoc		As Numeric 
    Local nPosRec		As Numeric 
    Local nY			As Numeric 
    Local oDlg          As Object 
    Local oPanel1       As Object 
    Local oPanel2       As Object 


    Default __lMetric  := FwLibVersion() >= "20210517"

    aArea      	:= GetArea()
    nOpca      	:= 0
    cTitulo	 	:= STR0189
    aHead 		:= {}
    aCol	 	:= {}
    cCampos 	:= "FRD_DOCUM,FRD_DESCRI,FRD_RECEB" 		// Campos a serem conciderados
    nX			:= 0
    nCols    	:= 0
    cLinOk		:= "FA050LinFRD()"							// Funcao de validacao da linha do grid (aCols)
    aAlter    	:= {}                                      	// Campos a serem alterados pelo usuario
    nFreeze    	:= 000              						// Campos estaticos na GetDados.
    nMax       	:= 999              						// Numero maximo de linhas permitidas.
    cFieldOk   	:= "AllwaysTrue"							// Funcao executada na validacao do campo
    cSuperDel   := "AllwaysTrue"          				    // Funcao executada quando pressionada as teclas <Ctrl>+<Delete>
    cDelOk      := "AllwaysTrue"    						// Funcao executada para validar a exclusao de uma linha do aCols
    aSizeTela	:= {}
    aObjects 	:= {}
    aInfo       := {}
    oDlg        := Nil
    oPanel1     := Nil
    oPanel2     := Nil 
    aPosObj     := {}
    nPosDoc		:= 0
    nPosRec		:= 0
    lIgual		:= .F.
    nY			:= 0
    lAchou		:= .F.
    lF050DOCS	:= ExistBlock("F050DOCS")    

    If SE2->E2_TEMDOCS == "1"

        If __lMetric
            // Metrica de controle de acessos 
            FwCustomMetrics():setSumMetric(Alltrim(ProcName()), "financeiro-protheus_qtd-por-acesso_total", 1)
        Endif

        aSizeTela := MsAdvSize()
        aadd( aObjects, {  30,  70, .T., .T.} )
        aadd( aObjects, {  20, 180, .T., .T., .T. } )
        aInfo := { aSizeTela[1],aSizeTela[2],aSizeTela[3],aSizeTela[4], 0, 0 }
        aPosObj := MsObjSize( aInfo, aObjects )

        DEFINE FONT oBold NAME "Arial" SIZE 0, -13 BOLD
        DEFINE MSDIALOG oDlg TITLE cTitulo FROM aSizeTela[7],0 TO aSizeTela[6],aSizeTela[5] OF oMainWnd PIXEL
        oDlg:lMaximized := .T.
        oTela     := FWFormContainer():New( oDlg )
        cIdBrowse := oTela:CreateHorizontalBox( 08 )
        cIdRodape := oTela:CreateHorizontalBox( 86 )
        oTela:Activate( oDlg, .F. )

        oPanel1  := oTela:GeTPanel( cIdBrowse )
        oPanel2  := oTela:GeTPanel( cIdRodape )

        @ 008, 015 Say STR0188 + SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA) SIZE 190,10 OF oPanel1 PIXEL FONT oBold COLOR CLR_BLUE

        // criar aHeader
        SX3->(dbSetOrder(1))
        SX3->(dbSeek("FRD"))
        While SX3->(!EOF()) .And.  SX3->X3_ARQUIVO == "FRD"
            If X3USO(SX3->X3_USADO) .And.  AllTrim(SX3->X3_CAMPO) $ Alltrim(cCampos) .AND. (cNivel >= SX3->X3_NIVEL)
                aAdd( aHead, { AlLTrim( X3Titulo() ), ; 	// 01 - Titulo
                SX3->X3_CAMPO	, ;		// 02 - Campo
                SX3->X3_Picture	, ;		// 03 - Picture
                SX3->X3_TAMANHO	, ;		// 04 - Tamanho
                SX3->X3_DECIMAL	, ;		// 05 - Decimal
                SX3->X3_Valid  	, ;		// 06 - Valid
                SX3->X3_USADO  	, ;		// 07 - Usado
                SX3->X3_TIPO   	, ;		// 08 - Tipo
                SX3->X3_F3		, ;		// 09 - F3
                SX3->X3_CONTEXT	, ;   	// 10 - Contexto
                SX3->X3_CBOX	, ; 	// 11 - ComboBox
                SX3->X3_RELACAO	, } )	// 12 - Relacao
            Endif
            SX3->(dbSkip())
        End

        // Criar Acols
        dbSelectArea("FRD")
        DbSetOrder(1)
        dbSeek(xFilial("FRD")+SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA))
        While !eof() .and. SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA) == FRD->(FRD_PREFIX+FRD_NUM+FRD_PARCEL+FRD_TIPO+FRD->FRD_FORNEC+FRD_LOJA)
            aAdd(aCol,Array(Len(aHead)+1))
            nCols ++

            For nX := 1 To Len(aHead)
                If ( aHead[nX][10] != "V")
                    aCol[nCols][nX] := FieldGet(FieldPos(aHead[nX][2]))
                Else
                    aCol[nCols][nX] := CriaVar(aHead[nX][2],.T.)
                Endif
            Next nX
            aCol[nCols][Len(aHead)+1] := .F.
            dbSkip()
        End

        aAdd(aAlter,'FRD_DOCUM')
        aAdd(aAlter,'FRD_RECEB')

        oGet			:= 	MsNewGetDados():New(0,0,170,402,GD_INSERT+GD_UPDATE+GD_DELETE,;
        cLinOk,,"",aAlter,nFreeze,nMax,cFieldOk,cSuperDel,cDelOk,oPanel2,aHead,aCol)
        oGet:oBrowse:Align := CONTROL_ALIGN_ALLCLIENT

        ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar(oDlg, {|| IIF(FA050LinFRD(),(nOpca:=1,oDlg:End()),)},{||oDlg:End()}) CENTERED

        If nOpcA == 1

            nPosDoc := ASCAN(oGet:aHeader,{|x| AllTrim(x[2])=="FRD_DOCUM"})
            nPosRec := ASCAN(oGet:aHeader,{|x| AllTrim(x[2])=="FRD_RECEB"})

            dbSelectArea("FRD")
            dbSetOrder(1)		//FRD_FILIAL+FRD_PREFIX+FRD_NUM+FRD_PARCEL+FRD_TIPO+FRD_FORNEC+FRD_LOJA+FRD_DOCUM
            dbGoTop()

            //Verifica se o aCols foi alterado, para então apagar o registro antigo
            If dbSeek(xFilial("SE2")+SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA))
                While !EoF() .And. FRD->(FRD_FILIAL+FRD_PREFIX+FRD_NUM+FRD_PARCEL+FRD_TIPO+FRD_FORNEC+FRD_LOJA) == xFilial("SE2")+SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)
                    For nX := 1 To Len(oGet:aCols)
                        If FRD->FRD_DOCUM == oGet:aCols[nX][nPosDoc]
                            lAchou := .T.
                        EndIf
                    Next nX
                    If !lAchou
                        RecLock("FRD",.F.)
                        dbDelete()
                        MsUnLock()
                    EndIf
                    lAchou := .F.
                    dbSkip()
                End
            EndIf

            For nX := 1 To Len(oGet:aCols)
                If !oGet:aCols[nx][Len(oGet:aHeader)+1]
                    If dbSeek(xFilial("SE2")+SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)+oGet:aCols[nx][nPosDoc])
                        RecLock("FRD",.F.)
                    Else
                        RecLock("FRD",.T.)
                    EndIf
                    FRD->FRD_FILIAL 	:= xFilial("FRD")
                    FRD->FRD_PREFIX 	:= SE2->E2_PREFIXO
                    FRD->FRD_NUM		:= SE2->E2_NUM
                    FRD->FRD_PARCEL 	:= SE2->E2_PARCELA
                    FRD->FRD_TIPO 		:= SE2->E2_TIPO
                    FRD->FRD_FORNEC 	:= SE2->E2_FORNECE
                    FRD->FRD_LOJA 		:= SE2->E2_LOJA
                    FRD->FRD_DOCUM		:= oGet:aCols[nX][nPosDoc]
                    FRD->FRD_RECEB		:= oGet:aCols[nX][nPosRec]
                    MsUnLock()
                Else
                    For nY := 1 To Len(oGet:aCols)
                        If !oGet:aCols[nY,Len(oGet:aHeader)+1] .And. (oGet:aCols[nX][nPosDoc] == oGet:aCols[nY][nPosDoc])
                            lIgual := .T.
                        EndIf
                    Next nY
                    If !lIgual
                        dbSelectArea("FRD")
                        dbSetOrder(1)		//FRD_FILIAL+FRD_PREFIX+FRD_NUM+FRD_PARCEL+FRD_TIPO+FRD_FORNEC+FRD_LOJA+FRD_DOCUM
                        If dbSeek(xFilial("SE2")+SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)+oGet:aCols[nx][nPosDoc])
                            RecLock("FRD",.F.)
                            dbDelete()
                            MsUnLock()
                        EndIf
                    EndIf
                EndIf
            Next

            // Ponto de entrada apos gravar a manutencao nos
            // documentos.
            // PARAMIXB[1] caracter com chave 1 da tabela SE2
            // PARAMIXB[2] array com acols   da tabela FRD
            // PARAMIXB[3] array com aheader da tabela FRD
            // Nao tem retorno
            IF lF050DOCS
                ExecBlock("F050DOCS",.f.,.f.,{xFilial("SE2")+SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA),oGet:aCols,oGet:aHeader})
            Endif

        EndIf

        RestArea( aArea )
    Else
        Help(" ",1,"FA050VDOC",,,1,0)	//"Controle de documentos não disponível para o título. O título não possui vinculo com tipos de documentos."
    EndIf

Return

//-------------------------------------------------------------------
/*/{Protheus.doc} FA050Contr
Rotina para rastreio de contratos a partir do titulo

@author Andre Lago
@since  21/07/10
/*/
//-------------------------------------------------------------------
Function FA050Contr(cAlias As Character, nReg As Numeric, nOpc As Numeric )

    LOCAL aAreaCN9    as Array
    LOCAL aAreaCND    as Array
    LOCAL aAreaSC7    as Array
    LOCAL aAreaSD1    as Array
    LOCAL aAreaSF1    as Array
    LOCAL aContratos  as Array
    LOCAL aPedidos    as Array
    LOCAL aRastrContr as Array
    LOCAL aTitCampos  as Array
    LOCAL cAliasSD1   as Character
    LOCAL cAliasSF1   as Character
    LOCAL cFornece    as Character
    LOCAL cLojaFor    as Character
    LOCAL cNota       as Character
    LOCAL cPrefixo    as Character
    LOCAL cQuery      as Character
    LOCAL cSerie      as Character
    LOCAL nOpcCtr     as Numeric
    LOCAL nPos        as Numeric
    LOCAL nX          as Numeric
    LOCAL oDlgCtr     as Object
    LOCAL oLbxCtr     as Object
    LOCAL oNo         as Object
    LOCAL oOk         as Object

    Default __lMetric  := FwLibVersion() >= "20210517"

    aAreaCN9   := CN9->(GetArea())
    aAreaSC7   := SC7->(GetArea())
    aAreaSD1   := SD1->(GetArea())
    aAreaSF1   := SF1->(GetArea())
    aAreaCND   := CND->(GetArea())
    cPrefixo   := ""
    cAliasSF1  := "SF1"
    cAliasSD1  := "SD1"
    cNota      := ""
    cSerie     := ""
    cFornece   := ""
    cLojaFor   := ""
    aPedidos   := {}
    aContratos := {}
    aRastrContr:= {}
    oDlgCtr    := Nil 
    oLbxCtr    := Nil
    aTitCampos := {" ", STR0192, STR0193, STR0194, STR0195}
    oOk        := LoadBitMap(GetResources(), "LBOK")
    oNo        := LoadBitMap(GetResources(), "LBNO")
    nOpcCtr    := 0
    nPos       := 0
    nX         := 0
    cQuery     := ""

    //Busca notas fiscais de entrada relacionadas com o titulo em questao:

    cAliasSF1 := "SF1TMP"
    cQuery	  := "  SELECT F1_FILIAL, F1_FORNECE, F1_LOJA, F1_DOC, F1_PREFIXO, F1_DOC, F1_SERIE, F1_FORNECE, F1_LOJA FROM " + RetSqlName('SF1')
    cQuery	  += "  WHERE F1_FILIAL  = '" + xFilial('SF1') + "' AND "
    cQuery	  += "    F1_FORNECE = '" + SE2->E2_FORNECE + "' AND"
    cQuery	  += "    F1_LOJA = '" + SE2->E2_LOJA + "' AND"
    cQuery	  += "    F1_DOC = '" + SE2->E2_NUM + "' AND"
    cQuery	  += "    D_E_L_E_T_ = ' '"
    cQuery    := ChangeQuery(cQuery)
    dbUseArea ( .T., "TOPCONN", TCGENQRY(,,cQuery), cAliasSF1, .F., .T.)

    While (cAliasSF1)->(!Eof() .AND. F1_FILIAL+F1_FORNECE+F1_LOJA+F1_DOC == xFilial('SF1')+SE2->(E2_FORNECE+E2_LOJA+E2_NUM))
        cPrefixo := If(Empty((cAliasSF1)->F1_PREFIXO),&(GetMV("MV_2DUPREF")),(cAliasSF1)->F1_PREFIXO)
        If cPrefixo == SE2->E2_PREFIXO
            cNota     := (cAliasSF1)->F1_DOC
            cSerie    := (cAliasSF1)->F1_SERIE
            cFornece  := (cAliasSF1)->F1_FORNECE
            cLojaFor  := (cAliasSF1)->F1_LOJA
            Exit
        Endif
        (cAliasSF1)->(DbSkip())
    EndDo

    (cAliasSF1)->(dbCloseArea())

    If Empty(cNota)
        Help(" ",1, "FA050NOTA",, STR0196 , 4,0)

        RestArea(aAreaSF1)
        RestArea(aAreaSD1)
        RestArea(aAreaSC7)
        RestArea(aAreaCN9)
        Return
    Endif

    //Busca Pedidos de Compras relacionados com a Nota de Entrada:

    cAliasSD1 := "SD1TMP"
    cQuery	  := "  SELECT * FROM " + RetSqlName('SD1')
    cQuery	  += "  WHERE D1_FILIAL  = '" + xFilial('SD1') + "' AND "
    cQuery	  += "    D1_DOC = '" + cNota + "' AND"
    cQuery	  += "    D1_SERIE = '" + cSerie + "' AND"
    cQuery	  += "    D1_FORNECE = '" + cFornece + "' AND"
    cQuery	  += "    D1_LOJA = '" + cLojaFor + "' AND"
    cQuery	  += "    D1_PEDIDO <> ' ' AND"
    cQuery	  += "    D_E_L_E_T_ = ' '"
    cQuery    := ChangeQuery(cQuery)
    dbUseArea ( .T., "TOPCONN", TCGENQRY(,,cQuery), cAliasSD1, .F., .T.)

    While (cAliasSD1)->(!Eof() .AND. D1_FILIAL+D1_DOC+D1_SERIE+D1_FORNECE+D1_LOJA == xFilial('SD1')+cNota+cSerie+cFornece+cLojaFor)
        If !Empty((cAliasSD1)->D1_PEDIDO)
            nPos := Ascan(aPedidos,{|x| x[01]+x[02] == (cAliasSD1)->(D1_PEDIDO+D1_ITEMPC)})
            If nPos == 0
                aadd(aPedidos,{(cAliasSD1)->D1_PEDIDO,(cAliasSD1)->D1_ITEMPC})
            Endif
        Endif
        (cAliasSD1)->(DbSkip())
    EndDo

    (cAliasSD1)->(dbCloseArea())

    If Empty(aPedidos)
        Help(" ",1, "FA050PEDI",, STR0196 , 4,0)

        RestArea(aAreaSF1)
        RestArea(aAreaSD1)
        RestArea(aAreaSC7)
        RestArea(aAreaCN9)
        Return
    Endif

    //Busca os contratos relacionados ao Pedido de Compras:
    CN9->(DbSetOrder(1))
    SC7->(DbSetOrder(1))
    For nX:=1 to Len(aPedidos)
        If SC7->(DbSeek(xFilial("SC7") + aPedidos[nX,01] + aPedidos[nX,02])) .AND. !Empty(SC7->C7_CONTRA)
            nPos := Ascan(aContratos,{|x| x[02] + x[03] == SC7->(C7_CONTRA+C7_CONTREV)})
            If nPos == 0
                CND->(DbSetOrder(7)) //CND_FILIAL+CND_CONTRA+CND_REVISA+CND_NUMMED
                If CND->(DbSeek(xFilial("CND") + SC7->C7_CONTRA + SC7->C7_CONTREV + SC7->C7_MEDICAO))
                    If CN9->(DbSeek(xFilial("CN9", CND->CND_FILCTR) + SC7->C7_CONTRA + SC7->C7_CONTREV))
                        aAdd(aContratos,{oNo, SC7->C7_CONTRA, SC7->C7_CONTREV, CN9->CN9_DTINIC, CN9->CN9_DTFIM, CND->CND_FILCTR})
                    Endif
                EndIf
            Endif
        Endif
    Next

    If Empty(aContratos)
        Help(" ",1, "FA050CNTR",, STR0196 , 4,0)

        RestArea(aAreaSF1)
        RestArea(aAreaSD1)
        RestArea(aAreaSC7)
        RestArea(aAreaCN9)
        RestArea(aAreaCND)
        Return
    Endif

    If Len(aContratos) == 1
        aRastrContr := aClone(aContratos)
    Else
        DEFINE MSDIALOG oDlgCtr FROM 50,40 TO 285,541 TITLE STR0198 Of oMainWnd PIXEL

        oLbxCtr := TWBrowse():New( 27,4,243,86,,aTitCampos,,oDlgCtr,,,,,,,,,,,,.F.,,.T.,,.F.,,,)
        oLbxCtr:SetArray(aContratos)
        oLbxCtr:bLDblClick := { || aContratos[oLbxCtr:nAt,1] := If(aContratos[oLbxCtr:nAt,1]:cName=="LBNO", oOk,oNo) }
        oLbxCtr:bLine := { || {aContratos[oLbxCtr:nAT][1],aContratos[oLbxCtr:nAT][2],aContratos[oLbxCtr:nAT][3],aContratos[oLbxCtr:nAT][4],aContratos[oLbxCtr:nAT][5]}}
        oLbxCtr:Align := CONTROL_ALIGN_ALLCLIENT

        ACTIVATE MSDIALOG oDlgCtr CENTERED ON INIT EnchoiceBar(oDlgCtr,{||If(VldSelCtr(oLbxCtr:aArray,aContratos),(nOpcCtr := 1,oDlgCtr:End()),oDlgCtr:End())},{||(nOpcCtr := 0,oDlgCtr:End())})

        If nOpcCtr == 1
            For nX:=1 to Len(aContratos)
                If aContratos[nX,01]:cName == "LBOK"
                    aAdd(aRastrContr, aContratos[nX])
                Endif
            Next
        Endif
    Endif

    If !Empty(aRastrContr)

        If __lMetric
            // Metrica de controle de acessos 
            FwCustomMetrics():setSumMetric(Alltrim(ProcName()), "financeiro-protheus_qtd-por-acesso_total", 1)
        Endif

        CNTC010( aRastrContr )
    Endif

    RestArea(aAreaSF1)
    RestArea(aAreaSD1)
    RestArea(aAreaSC7)
    RestArea(aAreaCN9)
    RestArea(aAreaCND)

Return

//-------------------------------------------------------------------
/*/{Protheus.doc} VldSelCtr
Valida selecao do contrato.

@author TOTVS
@since  16/11/10
/*/
//-------------------------------------------------------------------
Static Function VldSelCtr(aLbxCtr,aContratos)

    LOCAL nSelOK := 0

    aEval(aLbxCtr,{|x| If(x[1]:cName == "LBOK",++nSelOK,0)})

    If nSelOK == 0
        Help(" ",1, "FA050VLDC",, STR0199, 4,0)

        Return .f.
    ElseIf nSelOK > 1
        Help(" ",1, "FA050VLDC",, STR0200, 4,0)

        Return .f.
    Endif
    aContratos := aClone(aLbxCtr)

Return .t.

//-------------------------------------------------------------------
/*/{Protheus.doc} FA050LinFRD
Funcao de validaco da linha da grid dos documentos
vinculados ao titulo.

@author Renan G. Alexandre
@since  02/21/11
/*/
//-------------------------------------------------------------------
Function FA050LinFRD()

    Local lRet		:= .T.
    Local nPosDoc	:= 0
    Local nPosRec	:= 0
    Local nX		:= 0
    Local nColDel	:= 0

    nPosDoc := ASCAN(oGet:aHeader, {|aCpos| AllTrim(aCpos[2]) == "FRD_DOCUM"})
    nPosRec := ASCAN(oGet:aHeader, {|aCpos| AllTrim(aCpos[2]) == "FRD_RECEB"})

    If (nPosDoc > 0) .And. (nPosRec > 0)
        For nX := 1 To Len(oGet:aCols)
            If !oGet:aCols[nX][Len(oGet:aHeader)+1]
                If Empty(AllTrim(oGet:aCols[nX][nPosDoc])) .Or. Empty(AllTrim(oGet:aCols[nX][nPosRec]))
                    lRet := .F.
                    Help(" ",1,"FA050DOC1")
                    Exit
                ElseIf (nX != oGet:nAt) .And. (AllTrim(oGet:aCols[nX][nPosDoc]) == AllTrim(oGet:aCols[oGet:nAt][nPosDoc]));
                .And. !oGet:aCols[oGet:nAt][Len(oGet:aHeader)+1]
                    Help(" ",1,"FA050DOC2")		//"Código de documento já informado, não permitido."##"Informe um código diferente."
                    lRet := .F.
                    Exit
                EndIf
            Else
                nColDel++
            EndIf
        Next nX
    EndIf

    If nColDel >= Len(oGet:aCols)
        lRet := .F.
        Help(" ",1,"FA050DOC3")		//"Tipo de documento não informado."##"Informe o tipo de documento a ser vinculado ao título."
    EndIf

Return(lRet)

//-------------------------------------------------------------------
/*/{Protheus.doc} FA50SfeExi
Verifica se já foi gerado o registro de imposto para a ordem de pago.

@author Rodrigo Gimenes
@since  29/07/11
/*/
//-------------------------------------------------------------------
Function FA50SfeExi(cChave,cTipo)

    Local aAreaAtu  := {}

    aAreaAtu 	:= GetArea()
    lRetorno 	:= .F.

    dbSelectArea("SFE")

    SFE->(DbSetOrder(4))

    If (SFE->(DbSeek(cChave)))
        While !SFE->(Eof())
            If SFE->FE_TPTIMP == cTipo
                lRetorno := .T.
                Exit
            EndIf
            SFE->(dbSkip())
            Loop
        EndDo
    Endif   

    RestArea(aAreaAtu)

Return(lRetorno)



//-------------------------------------------------------------------
/*/{Protheus.doc} FCriaFII
Funcao que cria os registros de relacionamento de titulos

Esta rotina tem como objetivo criar o registro relacionado ao titulo
aglutinador

@cEntOri: Alias do registro  do titulo origem
@cPrefOri: Prefixo Origem
@cNumOri: Numero Origem
@cParcOri: Parcela Origem
@cTipoOri: Tipo Origem
@cCfOri: Fornecedor Origem
@cLojaOri: Loja Origem
@cEntDes: Alias do registro  do titulo destino
@cPrefDes : Prefixo Destino
@cNumDes: Numero Destino
@cParcDes: Parcela Destino
@cTipoDes: Tipo Destino
@cCfDes: Fornecedor Destino
@cLojaDes: Loja Destino
@cFilDes: Filial destino
@cFIISeq: Sequencia FII

@author Mauricio Pequim Jr
@since  27/06/11
/*/
//-------------------------------------------------------------------
Function FCriaFII(cEntOri, cPrefOri, cNumOri, cParcOri, cTipoOri, cCfOri, cLojaOri, cEntDes, cPrefDes, cNumDes, cParcDes, cTipoDes, cCfDes, cLojaDes, cFilDes, cFIISeq )

    Local aArea := GetArea()
    Local aAreaAC9 := AC9->(GetArea())
    Local cAliasAC9 := GetNextAlias()

    RecLock("FII",.T.)
    FII->FII_FILIAL := xFilial("FII")
    FII->FII_ENTORI := cEntOri
    FII->FII_PREFOR := cPrefOri
    FII->FII_NUMORI := cNumOri
    FII->FII_PARCOR := cParcOri
    FII->FII_TIPOOR := cTipoOri
    FII->FII_CFORI  := cCfOri
    FII->FII_LOJAOR := cLojaOri

    FII->FII_ENTDES := cEntDes
    FII->FII_PREFDE := cPrefDes
    FII->FII_NUMDES := cNumDes
    FII->FII_PARCDE := cParcDes
    FII->FII_TIPODE := cTipoDes
    FII->FII_CFDES  := cCfDes
    FII->FII_LOJADE := cLojaDes
    FII->FII_FILDES := cFilDes
    FII->FII_SEQ    := cFIISeq
    FII->FII_ROTINA := FUNNAME()
    FII->FII_OPERAC := "  "
    FII->(MsUnlock())

    // Grava referencia do novo titulo para o arquivo no banco de conhecimento
    dbSelectArea("AC9")
    AC9->(dbSetOrder(2))
    BeginSQL Alias cAliasAC9
		SELECT AC9.AC9_CODENT, AC9.R_E_C_N_O_
		FROM	%Table:AC9% AC9
		WHERE	AC9.AC9_FILENT = %Exp:xFilial("SE2")%
		AND     AC9.AC9_CODENT =  %Exp:cPrefOri+cNumOri+cParcOri+cTipoOri+cCfOri+cLojaOri%
		AND     AC9.%NotDel%
	EndSQL

    // Grava referencia do novo titulo para o arquivo no banco de conhecimento
    While (cAliasAC9)->(!EOF())
        AC9->(DbGoTo((cAliasAC9)->R_E_C_N_O_))
        RecLock("AC9",.F.)
            AC9->AC9_CODENT := cPrefDes+cNumDes+cParcDes+cTipoDes+cCfDes+cLojaDes
        MsUnlock()
        (cAliasAC9)->(DbSkip())
    EndDo

    If Select(cAliasAC9) > 0
		(cAliasAC9)->(DbCloseArea())
	EndIf
    
    RestArea(aAreaAC9)
    RestArea(aArea)    
    
Return Nil

//-------------------------------------------------------------------
/*/{Protheus.doc} F050RetPR
Efetua o estorno de titulos provisorios.

@author Carlos A. Queiroz
@since 27/06/11
/*/
//-------------------------------------------------------------------
Function F050RetPR(nRecnoSE2)

    Local cWhileFII := ""
    Local cChaveAC9 := ""
    Local aAreaSE2	:= {}
    Local aAreaSA2	:= {}
    Local cChaveTit := ""

    PRIVATE lMsErroAuto := .F.

    Default nRecnoSE2  := 0

    If nRecnoSE2 > 0

        aAreaSE2	:= SE2->(GetArea())
        aAreaSA2	:= SA2->(GetArea())
        //Posiciona no titulo gerado pela substtituicao
        SE2->(dbGoTo(nRecnoSE2))
        cWhileFII := (xFilial("SE2")+"SE2"+SE2->E2_PREFIXO+SE2->E2_NUM+SE2->E2_PARCELA+SE2->E2_TIPO+SE2->E2_FORNECE+SE2->E2_LOJA)
        cChaveAC9 := xFilial("SE2")+SE2->E2_PREFIXO+SE2->E2_NUM+SE2->E2_PARCELA+SE2->E2_TIPO+SE2->E2_FORNECE+SE2->E2_LOJA

        dbselectarea("FII")
        dbsetorder(2)
        If dbseek(xFilial("FII")+"SE2"+SE2->E2_PREFIXO+SE2->E2_NUM+SE2->E2_PARCELA+SE2->E2_TIPO+SE2->E2_FORNECE+SE2->E2_LOJA)
            While cWhileFII == (FII->FII_FILDES+"SE2"+FII->FII_PREFDE+FII->FII_NUMDES+FII->FII_PARCDE+FII->FII_TIPODE+FII->FII_CFDES+FII->FII_LOJADE)
                //Posiciona no titulo provisório componente da substituição
                cChaveTit := FII->(FII_PREFOR + FII_NUMORI + FII_PARCOR + FII_TIPOOR + FII_CFORI + FII_LOJAOR)

                lMsErroAuto := .F.
                dbselectarea("SE5")
                dbsetorder(7)
                If dbseek(xFilial("SE5",FII->FII_FILIAL)+cChaveTit)
                    cFilAtu := cFilAnt
                    cFilAnt := SE5->E5_FILORIG
                    aVetor 	:= {{"E2_PREFIXO"	, SE5->E5_PREFIXO 		,Nil},;
                                {"E2_NUM"		, SE5->E5_NUMERO       	,Nil},;
                                {"E2_PARCELA"	, SE5->E5_PARCELA  		,Nil},;
                                {"E2_TIPO"	    , SE5->E5_TIPO     		,Nil},;
                                {"E2_FORNECE"   , SE5->E5_CLIFOR   		,Nil},;
                                {"E2_LOJA"	    , SE5->E5_LOJA     		,Nil},;
                                {"AUTMOTBX"	    , SE5->E5_MOTBX      	,Nil},;
                                {"AUTDTBAIXA"	, SE5->E5_DATA			,Nil},;
                                {"AUTDTCREDITO" , SE5->E5_DTDISPO		,Nil},;
                                {"AUTHIST"	    , STR0209+alltrim(SE5->E5_PREFIXO)+STR0210+alltrim(SE5->E5_NUMERO)+STR0211+alltrim(SE5->E5_PARCELA)+STR0212+alltrim(SE5->E5_TIPO)+"."	,Nil},; //"Estorno de Baixa referente a substituicao de titulo tipo Provisorio para Efetivo. Prefixo: "#", Numero: "#", Prc: "#", Tp: "
                                {"AUTVALREC"	, SE5->E5_VALOR		    ,Nil}}

                    MSExecAuto({|x,y| Fina080(x,y)},aVetor,5)

                    // Recupera MV_PAR (F12) da rotina FINA050
                    Pergunte("FIN050",.F.)

                    If lMsErroAuto
                        DisarmTransaction()
                        MostraErro()
                        Break
                    Endif

                    If Empty(SE2->E2_BAIXA) //Verificando o titulo provisório
                        // Restaura referencia do arquivo do banco de conhecimento para o PR
                        dbSelectArea("AC9")
                        AC9->(dbSetOrder(2))
                        If AC9->(dbSeek(xFilial("AC9") + "SE2" + cChaveAC9))
                            RecLock("AC9",.F.)
                            AC9->AC9_CODENT := cChaveTit
                            MsUnlock()
                        EndIf

                        Reclock("FII" ,.F.,.T.)
                        FII->(dbDelete())
                        FII->(MsUnlock())
                    EndIf

                    SE2->(DbSetOrder(1))
                    If SE2->(MsSeek( xFilial("SE2") + cChaveTit)) .And. SE2->E2_FLUXO == "S"
                        AtuSldNat(SE2->E2_NATUREZ, SE2->E2_VENCREA, SE2->E2_MOEDA, If(SE2->E2_TIPO $ MVPAGANT+"/"+MV_CPNEG,"3","2"), "P", SE2->E2_VALOR, SE2->E2_VLCRUZ,If(SE2->E2_TIPO $ MVABATIM, "-", "+"),,FunName(),"SE2",SE2->(Recno()))
                    EndIf

                    cFilAnt := cFilAtu
                EndIf
                FII->(DbSkip())
            EndDo
        EndIf

        RestArea(aAreaSE2)
        RestArea(aAreaSA2)
    EndIf

    FwFreeArray(aAreaSE2)
    FwFreeArray(aAreaSA2)

Return .T.

//-------------------------------------------------------------------
/*/{Protheus.doc} IntegDef
Função para integração via Mensagem Única Totvs.

@author  Felipe Raposo
@version P12.1.17
@since   07/05/2018
/*/
//-------------------------------------------------------------------
Static Function IntegDef(cXml, cTypeTrans, cTypeMsg, cVersion, cTransac)
Return FINI050(cXml, cTypeTrans, cTypeMsg, cVersion, cTransac)


//-------------------------------------------------------------------
/*/{Protheus.doc} PMSProjPms
Funcao para atualizar campo E2_PROJPMS na substitucao

@author Clovis Magenta
@since 04/06/12
/*/
//-------------------------------------------------------------------
Function PMSProjPms(aNtit)

    Local aArea		:= GetArea()
    Local aAreaSE2	:= SE2->(GetArea())
    Local nX			:= 0
    Default aNtit	:= {}

    dbSelectArea("SE2")

    For nX:= 1 to Len(aNtit)

        If aNtit[nX][14]
            SE2->(dbGoTo(aNtit[nX][13]))
            Reclock("SE2", .F.)
            SE2->E2_PROJPMS := "1"
            MsUnlock()
        Endif

    Next nX

    RestArea(aAreaSE2)
    RestArea(aArea)

Return

//-------------------------------------------------------------------
/*/{Protheus.doc} fa050DelRat
Deleta os registros de rateio ao clicar em Fechar, para não
ser gravada indevidamente

@author Clovis Magenta
@since 21/01/13
/*/
//-------------------------------------------------------------------
Static Function fa050DelRat()

    dbSelectArea("TMP")
    dbGotop()
    While TMP->(!Eof())
        If !TMP->CTJ_FLAG
            TMP->CTJ_FLAG := .T.		//Deleto as linhas
        EndIf
        TMP->(DBskip())
    EndDo

Return

//-------------------------------------------------------------------
/*/{Protheus.doc} F050AUTAFR (nOpc)
Rotina para gravação automatica de rateio de projetos
@param nOpc - Opção da rotina (3 inclusao, 4 alteração)
@return lRet - Retorna se foi possível ou não incluir o rateio
@since 	15/02/2013
@version 	P11
/*/
//-------------------------------------------------------------------
Function F050AutAFR(nOpc)

    Local lRet:=.T.

    If FwIsInCallStack("EnchAuto")
        PmsDlgFI(nOpc,M->E2_PREFIXO,M->E2_NUM,M->E2_PARCELA,M->E2_TIPO,M->E2_FORNECE,M->E2_LOJA,.F.)
        IF Empty(aRatAFR)
            lRet:=.F.
        Endif
    Endif

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} BuscaSE5
Busca a baixa do titulo pai do desdobramento

@author Karen Honda
@since 15/04/13
/*/
//-------------------------------------------------------------------
Static Function BuscaSE5(cTipoDoc, cPrefixo, cNumero, cParcela, cTipo, cMotBx)

    Local lRet := .F.
    Local cQuery := ""

    cQuery := "SELECT E5_FILIAL,E5_TIPODOC,E5_PREFIXO,E5_NUMERO,E5_PARCELA,E5_TIPO "
    cQuery += "FROM " + RetSqlName("SE5")
    cQuery += " WHERE E5_FILIAL = '" + xFilial("SE5") + "' AND"
    cQuery += "	E5_TIPODOC = '" + cTipoDoc + "' AND"
    cQuery += "	E5_PREFIXO = '" + cPrefixo + "' AND"
    cQuery += "	E5_NUMERO = '"  + cNumero  + "' AND"
    cQuery += "	E5_PARCELA = '" + cParcela + "' AND"
    cQuery += "	E5_TIPO = '"    + cTipo    + "' AND"
    cQuery += "	E5_MOTBX = 'DSD' AND D_E_L_E_T_ = ' '"

    DbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),"QRY",.T.,.T.)
    If QRY->(!Eof())
        lRet := .T.
    EndIf
    QRY->(DBCloseArea())

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc}FA050VLMV
Verifica se na inclusão de titulos, tipo PA.
A Natureza permite movimentação bancaria.

@author Thiago Malaquias
@since  27/05/2014
@version 12
/*/
//-------------------------------------------------------------------
Function FA050VLMV()

    Local lRet := .T.
    Local aArea := GetArea()

    If 	M->E2_TIPO $ MVPAGANT .And. Posicione("SED",1,xfilial("SED") + M->E2_NATUREZ,"ED_MOVBCO") == "2"
        Help(" ",1,"FA050VLMV", , STR0234,1,0) //"A natureza não permite movimento bancário"
        lRet:=.F.
    EndIf

    RestArea(aArea)

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} F50VldBCOF
Função de validação dos dados bancários do fornecedor do título a pagar
@author Marylly Araújo Silva
@since 09/06/2014
@version P1180
@return Retorno Booleano da validação dos dados da conta bancária do fornecedor no título a pagar
/*/
//-------------------------------------------------------------------
Function F50VldBCOF()

    Local lRet			:= .T.
    Local aArea		:= GetArea()
    Local aFILArea	:= {}
    Local aSA2Area	:= {}
    Local cFilFIL		:= FWXFilial("FIL")
    Local lClosed		:= .F.

    If !EMPTY( M->E2_FORBCO + M->E2_FORAGE + M->E2_FORCTA )
        DbSelectArea( "FIL" ) //Contas Bancárias de Fornecedores
        aFILArea := FIL->( GetArea() )
        FIL->( DbSetOrder(1) ) //Filial + Fornecedor + Loja  + Tipo + Banco + Agencia + Conta

        DbSelectArea( "SA2" )
        aSA2Area := SA2->( GetArea() ) //Cadastro de Fornecedores
        SA2->( DbSetOrder(1) ) //Filial + Fornecedor + Loja

        /*
        * Se o fornecedor estiver preenchido.
        */
        If SA2->( msSeek( FWXFilial("SA2") + M->E2_FORNECE + M->E2_LOJA ) )
            If AllTrim(SA2->A2_BANCO) <> AllTrim(M->E2_FORBCO) .OR. AllTrim(SA2->A2_AGENCIA) <> AllTrim(M->E2_FORAGE) .OR. AllTrim(SA2->A2_NUMCON) <> AllTrim(M->E2_FORCTA)
                If FIL->( msSeek( cFilFIL + M->E2_FORNECE + M->E2_LOJA ) )
                    While FIL->( !Eof() .AND. cFilFIL + AllTrim(M->E2_FORNECE) + AllTrim(M->E2_LOJA) == FIL->FIL_FILIAL + AllTrim(FIL->FIL_FORNEC) + AllTrim(FIL->FIL_LOJA) )
                        If AllTrim(M->E2_FORBCO) + AllTrim(M->E2_FORAGE) + AllTrim(M->E2_FORCTA) == AllTrim(FIL->FIL_BANCO) + AllTrim(FIL->FIL_AGENCI) + AllTrim(FIL->FIL_CONTA)
                            If cPaisLoc == "RUS" .And. FIL->FIL_CLOSED == "1"
                                Help("",1,"FA050BANKCLOSED") //This bank is closed and cannot be used.
                                lRet := .F.
                                lClosed := .T.
                            Else
                                lRet := .T.
                            Endif
                            EXIT
                        Else
                            lRet := .F.
                        EndIf
                        FIL->( DbSkip() )
                    EndDo
                ElseIf !EMPTY( M->E2_FORBCO + M->E2_FORAGE + M->E2_FORCTA )
                    lRet := .F.
                EndIf

                If !lRet .And. !lClosed
                    Help( ,, 'F50VldBCOF',,STR0236 + CRLF + STR0237, 1, 0) //'Dados bancários do fornecedor inexistente no cadastro.' // 'Por favor, regularize as contas bancárias no cadastro de Fornecedores.'
                EndIf
            EndIf
        EndIf

        RestArea(aFILArea)
        RestArea(aSA2Area)
    EndIf
    RestArea(aArea)

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} F050VlCpos
Função de validação dos campos digitados em memoria em busca de caracteres especiais
@author TOTVS S/A
@since 13/06/2014
@version P1180
@return Retorno Booleano da validação dos dados
/*/
//-------------------------------------------------------------------
Function F050VlCpos()

    Local nX := 1
    Local aStruct := SE2->( dbStruct() )
    Local lOk := .T.
    Local cCposVld := "|E2_FILIAL|E2_PREFIXO|E2_NUM|E2_PARCELA|E2_TIPO|E2_FORNECE|E2_LOJA|" //Campos Considerados na validacao (Campos Chave da tabela)
    Do While nX <= Len(aStruct) .And. lOk
        If Upper(aStruct[nX][2]) == "C" .And. Upper(Alltrim(aStruct[nX][1])) $ cCposVld
            If CHR(39) $ M->&(Alltrim(aStruct[nX][1]))	 .Or. ;
            CHR(34) $ M->&(Alltrim(aStruct[nX][1]))
                lOk := .F.
            Endif
        Endif
        nX++
    Enddo
    If !lOk
        Help("",1,"INVCAR",,STR0235,1,0) //"Informe caracteres válidos no preenchimento dos campos"
    Endif

Return lOk


//-------------------------------------------------------------------
/*/{Protheus.doc} F050VldVlr
Verifica se o valor do título está negativo.
@author Daniel Mendes
@since 18/11/2014
@version P12
/*/
//-------------------------------------------------------------------
Function F050VldVlr()

    Local lRet       := .T.
    Local lPCCBaixa  := SuperGetMv("MV_BX10925",.T.,"2") == "1"
    Local lIRPFBaixa := IIf(__lLocBRA , SA2->A2_CALCIRF == "2", .F.)        // Controla IRPF na Baixa
    Local lCalcIssBx := IsIssBx("P")
    Local nCasDec    := MsDecimais(1)
    Local nValor     := NoRound(xMoeda(M->E2_VALOR + M->E2_ACRESC, M->E2_MOEDA, 1, M->E2_EMISSAO, (nCasDec + 1), M->E2_TXMOEDA), nCasDec)
    Local nDecresc   := NoRound(xMoeda(M->E2_DECRESC, M->E2_MOEDA, 1, M->E2_EMISSAO, (nCasDec + 1), M->E2_TXMOEDA), nCasDec)
    
    If  !(M->E2_TIPO $ MVPROVIS) .and. ( nValor ) - ( Iif( lCalcIssBx , M->E2_ISS , 0 ) +;
        Iif( lIRPFBaixa , M->E2_IRRF , 0 ) +;
        Iif( lPCCBaixa , M->E2_CSLL + M->E2_COFINS + M->E2_PIS , 0 ) +;
        nDecresc ) <= 0
        If !lF050auto
            MsgAlert( STR0243 , STR0115 )
            lRet := .F.
        Else
            AutoGRLog(STR0243)
            lRet := .F.
        Endif        
    EndIf

    If lRet
        IF IIF(Type("ALTERA") == "U", .F., ALTERA)
            If nOldSaldo !=  SE2->E2_SALDO
                nOldSaldo :=  SE2->E2_SALDO
            EndIf
        EndIf
    EndIf

Return lRet


//-------------------------------------------------------------------
/*/{Protheus.doc} F050TitRet
Verifica se o título retentor de impostos foi gerado em outro módulo.
@author TOTVS S/A
@since 05/01/2015
@version P12
@return Retorno Booleano da validação dos dados
/*/
//-------------------------------------------------------------------
Function F050TitRet()

    Local aArea		:= {}
    Local aSE2		:= {}
    Local lRet := .F.
    Local lSFQ		:= .F.
    Local cQuery	:= ""
    Local cAliasSE2	:= ""
    Local lPCCBaixa	:= SuperGetMv("MV_BX10925",.T.,"2") == "1"

    If !lPCCBaixa
        aArea := GetArea()
        aSe2 := SE2->(GetArea())
        //Busco a informacao de qual o titulo retentor do PCC do titulo em alteracao
        SFQ->(DbSetOrder(2)) //-- FQ_FILIAL+FQ_ENTDES+FQ_PREFDES+FQ_NUMDES+FQ_PARCDES+FQ_TIPODES+FQ_CFDES+FQ_LOJADES
        If SFQ->(DbSeek(xFilial("SFQ")+"SE2"+SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)))
            lSFQ := .T.
            //Posiciono no cadastro de C.Pagar para verificar se o titulo retentor
            //foi contabilizado ou veio de outro modulo
            SE2->(DbSetOrder(1)) //-- E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA
            If SE2->(DbSeek(xFilial("SE2")+SFQ->(FQ_PREFORI+FQ_NUMORI+FQ_PARCORI+FQ_TIPOORI+FQ_CFORI+FQ_LOJAORI)))
                //Titulos contabilizados
                //os titulos vindos de outros modulos sempre tem E2_LA = 'S' ja que a contabilizacao ocorre na origem
                If !(AllTrim(SE2->E2_ORIGEM) == "FINA050") .Or. SE2->E2_LA == "S"
                    lRet := .T.
                Endif
            Endif
        Else
            //Verifico se o titulo eh retentor do PCC de outros titulos
            SFQ->(DbSetOrder(1)) //-- FQ_FILIAL+FQ_ENTORI+FQ_PREFORI+FQ_NUMORI+FQ_PARCORI+FQ_TIPOORI+FQ_CFORI+FQ_LOJAORI
            If SFQ->(DbSeek(xFilial("SFQ")+"SE2"+SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)))
                lSFQ := .T.
                //Titulos contabilizados
                //os titulos vindos de outros modulos sempre tem E2_LA = 'S' ja que a contabilizacao ocorre na origem
                If !(AllTrim(SE2->E2_ORIGEM) == "FINA050") .Or. SE2->E2_LA == "S"
                    lRet := .T.
                Endif
            Endif
        Endif
        If !lSFQ
            If !(AllTrim(SE2->E2_ORIGEM) == "FINA050") .Or. SE2->E2_LA == "S"
                cQuery := "select R_E_C_N_O_ from " + RetSQLName("SE2")
                cQuery += " where E2_FILIAL = '" + xFilial("SE2") + "'"
                cQuery += " and E2_NUM = '" + SE2->E2_NUM + "'"
                cQuery += " and E2_PREFIXO = '" + SE2->E2_PREFIXO + "'"
                cQuery += " and E2_TIPO in " + FormatIn(MVTAXA+"/"+MVTXA,"/")
                cQuery += " and ("
                cQuery += " E2_NATUREZ = '" + AllTrim(GetMv("MV_PISNAT")) + "' or"
                cQuery += " E2_NATUREZ = '" + AllTrim(GetMv("MV_COFINS")) + "' or"
                cQuery += " E2_NATUREZ = '" + AllTrim(GetMv("MV_CSLL")) + "'"
                cQuery += ")"
                cQuery += " and D_E_L_E_T_=' '"

                cQuery := ChangeQuery(cQuery)
                cAliasSE2 := GetNextAlias()
                DbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),cAliasSE2,.F.,.T.)
                lRet := !((cAliasSE2)->(Eof()))
                DbSelectArea(cAliasSE2)
                DbCloseArea()
            Endif
        Endif

        RestArea(aSE2)
        RestArea(aArea)
    Endif    
   
Return(lRet)

//-------------------------------------------------------------------
/*/{Protheus.doc} F050VERRAT
Validação do campo E2_RATEIO
@author TOTVS S/A
@since 24/02/2015
@version P12.1.4
@return Retorno Booleano da validação dos dados
/*/
//-------------------------------------------------------------------
FUNCTION F050VERRAT()

    Local lRet := .T.

    //E2_RATEIO
    If GetMv("MV_RATDESD",,"2") != "1" // Se nao rateia desdobramento
        lRet := (M->E2_DESDOBR == "N")
    Else
        lRet := (M->E2_MULTNAT != "1")
    Endif

RETURN lRet


//-------------------------------------------------------------------------------
/*/{Protheus.doc} F050VERMUL
Validação do campo E2_MULTNAT
@author TOTVS S/A
@since 24/02/2015
@version P12.1.4
@return Retorno Booleano da validação dos dados
/*/
//-------------------------------------------------------------------------------
FUNCTION F050VERMUL()

    Local lRet := .T.

    If GetMv("MV_RATDESD",,"2") == "1" // Se nao rateia desdobramento
        lRet := (MV_MULNATP .And. M->E2_RATEIO == "N")
    Endif

RETURN lRet

//--------------------------------------------------------------------------------
/*/{Protheus.doc} FInsDif
Verifica se o título de Inss teve sua natureza padrão alterada

@author TOTVS S/A
@since 04/06/2015
@version P11
@return Retorno Booleano da validação dos dados
/*/
//-------------------------------------------------------------------------------
Static Function FInsDif(cTitPai)

    Local aArea 	:= GetArea()
    Local lRet		:= .F.
    Local cQuery	:= ""
    Local cForInss	 := GetMv("MV_FORINSS")
    Local nTamFornc	:= TAMSX3("E2_FORNECE")[1]
    Local nTamLj		:= TAMSX3("E2_LOJA")[1]

    cQuery	:= "SELECT E2_NUM "
    cQuery	+= " FROM " + RetSqlName("SE2") + " SE2 "
    cQuery	+= " WHERE E2_NUM = '" + SE2->E2_NUM + "' "
    cQuery	+= " AND E2_TIPO = '" + MVINSS + "' "
    cQuery += " AND E2_PARCELA = '" + SE2->E2_PARCELA + "' "
    If Len(AllTrim(cForInss)) <= nTamFornc
        cQuery	+= " AND E2_FORNECE = '" + cForInss + "' "
    Else
        cQuery	+= " AND E2_FORNECE = '" + Substr(cForInss, 1, nTamFornc) + "' "
        cQuery	+= " AND E2_LOJA = '" + Substr(cForInss, nTamFornc + 1, nTamLj) + "' "
    EndIf
    cQuery	+= " AND RTRIM(LTRIM(E2_TITPAI)) = '" + cTitPai + "' "
    cQuery	+= " AND D_E_L_E_T_ = '' "

    cQuery := ChangeQuery(cQuery)

    DbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),"cAlias",.F.,.T.)
    lRet := !Empty(cAlias->E2_NUM)

    DbSelectArea("cAlias")
    DbCloseArea()
    RestArea(aArea)

Return lRet


//-------------------------------------------------------------------
/*/{Protheus.doc} FN50Log
Consulta log de atualizações
@author Rodolfo Novaes
@since  28/10/2015
@version 12
/*/
//-------------------------------------------------------------------
Function FN50Log()

    Local cIdDoc 	:= ""
    Local cChaveTit := ""
    Local cFilBkp   := cFilAnt

    Default __lMetric  := FwLibVersion() >= "20210517"
    
    If __lMetric
        // Metrica de controle de acessos 
        FwCustomMetrics():setSumMetric(Alltrim(ProcName()), "financeiro-protheus_qtd-por-acesso_total", 1)
    EndIF 

    cChaveTit := xFilial("SE2") + "|" + SE2->E2_PREFIXO + "|" + SE2->E2_NUM + "|" + SE2->E2_PARCELA + "|" +;
                        SE2->E2_TIPO + "|" + SE2->E2_FORNECE + "|" + SE2->E2_LOJA

    cIdDoc    := FINGRVFK7("SE2", cChaveTit)
    
    cFilAnt := SE2->E2_FILORIG

    ProcLogView( cFilAnt, cIdDoc)

    cFilAnt := cFilBkp

Return .T.

//-------------------------------------------------------------------
/*/{Protheus.doc} F050EasyOrig
Função para executar a chamado da função AvFlags na alteração
do titulo no financeiro
@author Laercio G Souza Jr
@since  18/05/16
@version 12
/*/
//-------------------------------------------------------------------
Function F050EasyOrig(cOrigem)
return EasyOrigem(cOrigem)

//-------------------------------------------------------------------
/*/{Protheus.doc} F050ExcTmp
Cria arquivo temporario para GetDb
@author Fabio Casagrande Lima
@since 02.01.17
@version 12.1.14
/*/
//-------------------------------------------------------------------
Static Function F050ExcTmp(cExclLP,lExclDsd)

    Local     aCpos     := {}
    Local     aAltera   := {}
    Private   cPrograma	:= "FINA050"
    Default   cExclLP   := "512"

    //Determina LP, já que é parametro para a função F050HeadCT
    IF !E2_TIPO $ MVPROVIS .or. mv_par02 == 1
        IF SE2->E2_TIPO $ MVPAGANT
            cExclLP := "514"
        Endif
        If lExclDsd //Desdobramento
            cExclLP:="578"
        Endif
    Endif

    aCpos := F050HeadCT(cExclLP,"FINA050",@aAltera,3) //Monta array com os campos a serem criados na tabela temporária

    F050Cria(aCpos) //Função responsavel por inserir a tabela temporária no banco de dados

Return

//-------------------------------------------------------------------
/*/{Protheus.doc} F050RatDes
Tratamento para When de dicionário
@author Totvs
@since 02.01.17
/*/
//-------------------------------------------------------------------
Function F050RatDes(nOpcCpo)

    Local lRet := .F.

    DEFAULT nOpcCpo := 0

    //Verifico se permite rateios de desdobramento no mesmo titulo
    If __lRatDsd == NIL
        __lRatDsd := IIF(GetMv("MV_RATDESD",,"2") == "1", .T., .F.)
    Endif

    lRet := __lRatDsd

    If nOpcCpo == 0
        lRet := .F.
    ElseIf !lRet
        If nOpcCpo == 1         //E2_MULTNAT
            lRet := (MV_MULNATP .AND. M->E2_DESDOBR == "N")
        ElseIf nOpcCpo == 2     //E2_RATEIO
            lRet := (M->E2_DESDOBR == "N" .AND. M->E2_MULTNAT == "2")
        ElseIf nOpcCpo == 3     //E2_DESDOBR
            lRet := (M->E2_RATEIO == "N" .AND. M->E2_MULTNAT == "2" )
        Endif
    Endif

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc}VldConcDda
//Função valida se o título a ser excluído possui conciliação DDA.
@author Sivaldo Oliveira
@since  20/02/2017
@version 12
/*/
//-------------------------------------------------------------------
Function VldConcDda(cFil, cForn, cLoja, cCodBar, cChaveDda)

    Local aAreaAt := GetArea()
    Local lRet := .F.

    cQry := "SELECT COUNT(FIG_DDASE2) NUMREG FROM " + RetSqlName("FIG")
    cQry += " WHERE FIG_FILIAL = '" + cFil + "' AND "
    cQry += "FIG_FORNEC = '" + cForn + "' AND "
    cQry += "FIG_LOJA = '" + cLoja + "' AND "
    cQry += "FIG_CODBAR = '" + cCodBar + "' AND "
    cQry += "FIG_CONCIL = '1' AND "
    cQry += "FIG_DDASE2 = '" + cChaveDda + "' AND "
    cQry += "D_E_L_E_T_ = ' ' "

    cQry := ChangeQuery(cQry)
    dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQry),"TMPFIG",.T.,.T.)
    TcSetField("TMPFIG","NUMREG"  ,"N", 17,2)

    lRet := TMPFIG->NUMREG > 0
    RestArea(aAreaAt)
    TMPFIG->(dbCloseArea())

Return lRet

//-----------------------------------------------------------------------------------------
/*/{Protheus.doc} F050VTitEmp
Ver se o titulo foi gerado por Emprestimo (metodo antigo antes da gravacao da Origem)
@author Pequim
@since 04/06/2015
@version P11
@return Retorno Booleano da validação dos dados
/*/
//-----------------------------------------------------------------------------------------
Static Function F050VTitEmp()

    Local lRet := .F.

    IF __nTamFor == NIL
        __nTamFor	:= TamSX3("E2_FORNECE")[1]
    Endif
    If __cForPar == NIL
        __cForPar	:= PadR(SuperGetMV("MV_FOREMPR",.F.,"000001"),__nTamFor)
    Endif
    If __nTaEHNum == NIL
        __nTaEHNum 	:= TAMSX3("EH_NUMERO")[1]
    EndIf

    If SE2->E2_PREFIXO == "EMP" .and. SE2->E2_TIPO == "PR "
        SEH->(dbSetOrder(1))
        If SEH->(MsSeek(xFilial("SEH")+PADR(SE2->E2_NUM,__nTaEHNum))) .and. SEH->EH_NATUREZ == SE2->E2_NATUREZ .and. SEH->EH_APLEMP == "EMP" .and. __cForPar == SE2->E2_FORNECE
            lRet := (SEH->EH_GERPARC == '1')
        EndIf
    EndIf

Return lRet

//-------------------------------------------------------
/*/{Protheus.doc} FA050PenC
Verifica se existem baixas pendentes de contabilizacao

@author Rodrigo Oliveira

@since 19/03/2015
@version P12
/*/
//-------------------------------------------------------
Function FA050PenC(aChave As Array) As Array

    Local aPenCont		As Array
    Local cQuery		As Character
    Local lRet			As Logical
    Local aArea			As Array
    Local cLAP			As Character
    Local cLAR			As Character

    If ValType(aChave) == "U"
        aChave := Array(7)
        aFill(aChave,"")
    Endif

    aPenCont := {}
    lRet := .F.
    cLAP := cLAR := 'S'
    aArea := SE5->(GetArea())

    cQuery := " SELECT P.LAP,P.RECNOP,R.LAR,R.RECNOR "
    cQuery += " FROM "
    cQuery += " (SELECT  E5_LA LAP, R_E_C_N_O_ RECNOP "
    cQuery += " FROM "+ RetSqlName( "SE5" ) + " SE5 "
    cQuery += " WHERE D_E_L_E_T_ = ' '  AND "
    cQuery += " E5_FILIAL = '" +aChave[1]+ "' AND "
    cQuery += " E5_PREFIXO = '" +aChave[2]+ "' AND "
    cQuery += " E5_NUMERO = '" +aChave[3]+ "' AND "
    cQuery += " E5_PARCELA = '" +aChave[4]+ "' AND "
    cQuery += " E5_TIPO = '" +aChave[5]+ "' AND "
    cQuery += " E5_CLIFOR = '" +aChave[6]+ "' AND "
    cQuery += " E5_LOJA = '" +aChave[7]+ "' AND "
    cQuery += " E5_SITUACA <> 'C' AND "
    cQuery += " E5_RECPAG = 'P')P,  "
    cQuery += " (SELECT  E5_LA LAR, R_E_C_N_O_ RECNOR "
    cQuery += " FROM "+ RetSqlName( "SE5" ) + " SE5 "
    cQuery += " WHERE D_E_L_E_T_ = ' '  AND "
    cQuery += " E5_FILIAL = '" +aChave[1]+ "' AND "
    cQuery += " E5_PREFIXO = '" +aChave[2]+ "' AND "
    cQuery += " E5_NUMERO = '" +aChave[3]+ "' AND "
    cQuery += " E5_PARCELA = '" +aChave[4]+ "' AND "
    cQuery += " E5_TIPO = '" +aChave[5]+ "' AND "
    cQuery += " E5_CLIFOR = '" +aChave[6]+ "' AND "
    cQuery += " E5_LOJA = '" +aChave[7]+ "' AND "
    cQuery += " E5_SITUACA <> 'C' AND "
    cQuery += " E5_RECPAG = 'R')R  "

    If Select("TSQL") > 0
        dbSelectArea("TSQL")
        DbCloseArea()
    EndIf

    dbUseArea(.T.,"TOPCONN",TCGENQRY(,,cQuery),"TSQL",.F.,.T.)

    dbSelectArea("TSQL")
    TSQL->(dbGotop())
    Do While TSQL->(!Eof())
        TCSetField("TSQL", "RECNOP" ,"N",16,0)
        TCSetField("TSQL", "RECNOR" ,"N",16,0)
        cLAP	:= If(alltrim(TSQL->LAP) <> 'S','N','S')
        cLAR	:= If(alltrim(TSQL->LAR) <> 'S','N','S')

        If alltrim(cLAP) <> alltrim(cLAR)
            If alltrim(cLAP) <> 'S'
                aAdd(aPenCont,TSQL->RECNOP)
            Else
                aAdd(aPenCont,TSQL->RECNOR)
            Endif
        Endif
        TSQL->(DbSkip())
    EndDo

    DbCloseArea()
    RestArea(aArea)

Return aPenCont

//-------------------------------------------------------
/*/{Protheus.doc} FA050PenC
Lista baixas pendentes de contabilizacao validando se
o processo tera continuidade via retorno logico.

@author Rodrigo Oliveira

@since 19/03/2015
@version P12
/*/
//-------------------------------------------------------
Function FA050MonP(aPenCont As Array, lDialog As Logical) As Logical

    Local lRet		 As Logical
    Local nX 		 As Numeric
    Local aReg		 As Array
    Local aArea		 As Array
    Local oDlg		 As Object
    Local cTit		 As Character
    Local cReg		 As Character
    Local cTxtRotAut As Character
    Local lF050DELC  As Logical

    Default aPenCont := {}
    Default lDialog  := .T.

    lRet	:= .T.
    nX		:= 0
    aReg	:= {}
    aArea	:= SE5->(GetArea())
    cReg	:= " { "
    cTit := cTxtRotAut := ""
    lF050DELC := ExistBlock("F050DELC")

    DbSelectArea("SE5")
    For nX := 1 to Len(aPenCont)
        If nX > 1
            cReg += " , "
        EndIf
        DbGoTo(aPenCont[nX])
        cReg += " { '" + Alltrim(E5_TIPODOC)+ "','"+ VerTpDoc(E5_TIPODOC)+ "',Val('" + Str(Round(E5_VALOR,2))+ "') ,'" + Alltrim(E5_SEQ)+ "','" +AllTrim(Str(aPenCont[nX])) + "' }	"
    Next nX
    cReg		+= " } "
    aReg		:= &(cReg)

    cTit := SE5->(E5_FILIAL + E5_PREFIXO + E5_NUMERO + E5_PARCELA + E5_TIPO  + E5_CLIFOR + E5_LOJA )
    If lDialog .And. !lF050Auto

        DEFINE MSDIALOG oDlg TITLE STR0026 FROM 180,180  TO 500,700 PIXEL
        @ 10, 10 TO 130,255 of oDlg PIXEL
        @ 20, 030 SAY STR0285 SIZE 170,10 of oDlg PIXEL
        @ 35, 030 SAY STR0286 SIZE 30,10 of oDlg PIXEL
        @ 35, 070 SAY cTit SIZE 100,10 of oDlg PIXEL
        @ 50, 030 SAY STR0287 SIZE 170,10 of oDlg PIXEL
        @ 25, 220 BUTTON STR0014 SIZE 030, 015 PIXEL OF oDlg ACTION (lRet := .T., oDlg:End())
        @ 45, 220 BUTTON STR0288 SIZE 030, 015 PIXEL OF oDlg ACTION (lRet := .F., oDlg:End())

        oBrowse := TWBrowse():New( 70 , 15, 235,50,,{STR0289,STR0290,STR0291,STR0292,'Recno'},{30,100,30,50,30},;
        oDlg,,,,,{||},,,,,,,.F.,,.T.,,.F.,,, )

        oBrowse:SetArray(aReg)
        oBrowse:bLine := &("{ || {aReg[oBrowse:nAt,01], aReg[oBrowse:nAt,02], aReg[oBrowse:nAt,03], aReg[oBrowse:nAt,04], aReg[oBrowse:nAt,05] } } ")
        oBrowse:lColDrag	:= .T.

        ACTIVATE MSDIALOG oDlg CENTERED
    Else
        lRet := .F.
        If lF050DELC
            lRet := Execblock("F050DELC",.F.,.F.,aReg)
        EndIf
        If !lRet
            cTxtRotAut := STR0293 + cTit + STR0294
            cTxtRotAut += STR0295 + CRLF + CRLF
            lMsErroAuto := .F.
            Help(" ",1,"PENCONT","FINA050 - " + STR0007,cTxtRotAut,1,0)
        EndIf
    EndIF

    RestArea(aArea)

Return lRet

//-------------------------------------------------------
/*/{Protheus.doc} F050AtuPFS
Atualiza informações referentes a integração SIGAPFS x SIGAFIN.

@author Jorge Martins

@since 09/03/2018
@version P12
/*/
//-------------------------------------------------------
Function F050AtuPFS(nOpc, nRecSE2, nRecSE5)

    Local lRet := .T.

    Default nRecSE2 := SE2->(Recno())
    Default nRecSE5 := SE5->(Recno())

    Do Case
        Case nOpc == 3 .And. FindFunction("JIncTitCP")
        lRet := JIncTitCP(nRecSE2, nRecSE5)

        Case nOpc == 4 .And. FindFunction("JAltTitCP")
        lRet := JAltTitCP(nRecSE2)

        Case nOpc == 5 .And. FindFunction("JDelTitCP")
        lRet := JDelTitCP(nRecSE2)
    EndCase

    If lRet .And. FindFunction("JDesdFilho")
        lRet := JDesdFilho(nOpc, nRecSE2)
    EndIf

Return lRet

//-------------------------------------------------------
/*/{Protheus.doc} FA050Va
Função de inclusão de valores acessórios para titulos CP

@author Marcos Gomes
@since  19/03/2015
@version P12

@return lRet	se o processo foi concluido com sucesso
/*/
//-------------------------------------------------------
Function Fa050VA(lVAAuto as Logical) As Logical

    Local oModelVA		:= NIL
    Local oSubFKD		:= NIL
    Local cChave		:= ""
    Local cIdDoc		:= ""
    Local cLog			:= ""
    Local lRet			:= .T.
    Local nX			:= 0
    Local nTamCod		:= 0
    Local aLinhasAlt    := {}

    DEFAULT lVAAuto	:= .F.
    DEFAULT  __lFIN50VA := FindFunction("FINA050VA")
    
    
        nTamCod	:=	TamSx3("FKD_CODIGO")[1]

        If lVAAuto
            //Rotina Automática para VA
            oModelVA := FWLoadModel('FINA050VA')
            oModelVA:SetOperation( 4 ) //Alteração
            oModelVA:Activate()

            oSubFKD := oModelVA:GetModel('FKDDETAIL')

            cChave := xFilial("SE2",SE2->E2_FILORIG) + "|" + SE2->E2_PREFIXO + "|" + SE2->E2_NUM + "|" + SE2->E2_PARCELA + "|" + SE2->E2_TIPO + "|" + SE2->E2_FORNECE + "|" + SE2->E2_LOJA
            cIdDoc := FINGRVFK7( 'SE2', cChave )
            oModelVA:LoadValue( "FK7DETAIL", "FK7_IDDOC", cIdDoc )

            If ALTERA
                // Controle para saber se haverá linhas no FKD que não foram passadas no msExecAuto.
                aLinhasAlt := array(oSubFKD:Length())
                aFill(aLinhasAlt, .F.)

                // Adiciona as linhas passadas no msExecAuto.
                For nX := 1 to Len(__aVAAuto)
                    If oSubFKD:SeekLine({{"FKD_CODIGO", Padr(__aVAAuto[nX, 1], nTamCod)}})
                        aLinhasAlt[oSubFKD:GetLine()] := .T.  // Marca a linha como atualizada.
                    Else
                        oSubFKD:AddLine()
                        oSubFKD:SetValue("FKD_CODIGO", Padr(__aVAAuto[nX, 1], nTamCod))
                    EndIf
                    oSubFKD:SetValue("FKD_VALOR", __aVAAuto[nX, 2])
                Next nX

                // Apaga as linhas não passadas no msExecAuto.
                For nX := 1 to len(aLinhasAlt)
                    If !aLinhasAlt[nX]
                        oSubFKD:GoLine(nX)
                        oSubFKD:DeleteLine()
                    Endif
                Next nX
            Else
                For nX := 1 to Len(__aVAAuto)
                    If !oSubFKD:IsEmpty()
                        oSubFKD:AddLine()
                    EndIf
                    oSubFKD:SetValue("FKD_CODIGO", Padr(__aVAAuto[nX,1],nTamCod) )
                    oSubFKD:SetValue("FKD_VALOR",  __aVAAuto[nX,2] )
                Next
            Endif

            If oModelVA:VldData()
                FWFormCommit( oModelVA )
            Else
                lRet	 := .F.
                cLog := cValToChar(oModelVA:GetErrorMessage()[4]) + ' - '
                cLog += cValToChar(oModelVA:GetErrorMessage()[5]) + ' - '
                cLog += cValToChar(oModelVA:GetErrorMessage()[6])
                Help( ,,"F050VALAC",,cLog, 1, 0 )
            Endif
            oModelVA:Deactivate()
            oModelVA:Destroy()
            oModelVA := NIL
        Else
            // Chamada com tela para cadastro de VA do títulos CP
            If __lFIN50VA .And. MsgYesNo( STR0314,STR0026)		//###STR0314 "Deseja cadastrar os valores acessórios deste título agora?"###STR0026 "Atençäo"
                FINA050VA()
            Endif
        Endif
   
Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} F050VldImp()

@author  Sivaldo Oliveira
@since 07/11/2017
@version 12
/*/
//-------------------------------------------------------------------
Function F050VldImp(lTelaRet)

    Local nZ As Numeric
    Local nPosPcc As Numeric
    Local nPosIrf As Numeric
    Local nPosIns As Numeric
    Local nPosIss As Numeric
    Local nPosCid As Numeric
    Local nPosSes As Numeric
    Local nImpos As Numeric
    Local aImpos As Array
    Local cChaveFK7 As Character
    Local cIdOriFKA As Character
    Local cFKM As Character
    Local lCalcImp As Logical
    Local cField As Character
    Local nBaseMR As Numeric
    Local nCasDec As Numeric
    Local nY As Numeric
    Local aOutImp As Array
    Local aImpConf As Array
    Local cLista As Character
    Local lCalcIssBx As Logical
    Local lIRPFBaixa As Logical
    Local lPCCBaixa As Logical
    Local lIsPA As Logical
    Local lRefazVlr As Logical
    Local nValorAux As Numeric

    Default lTelaRet := .F.

    //Inicializa as variáveis
    nZ := 0
    nPosPcc := 0
    nPosIrf := 0
    nPosIns := 0
    nPosIss := 0
    nPosCid := 0
    nPosSes := 0
    aImpos := {}
    nImpos := 0
    cChaveFK7 := ""
    cFKM := ""
    cField := AllTrim(ReadVar())
    nBaseMR := 0
    nCasDec := MsDecimais(1)
    nY := 0
    aOutImp := {}
    aImpConf := {}
    cLista := ""
    lCalcIssBx := IsIssBx("P")
    lIRPFBaixa := IIf(__lLocBRA, SA2->A2_CALCIRF == "2", .F.)
    lPCCBaixa := SuperGetMv("MV_BX10925",.T.,"2") == "1"
    __lPccBxMR := .F.
    __lIrfBxMR := .F.
    __nImpMR := 0
    lIsPA := (M->E2_TIPO $ MVPAGANT)
    lRefazVlr := .F.
    nValorAux := 0

    //Valida se o tipo cálcula impostos
    lCalcImp := !( M->E2_TIPO $ (MVABATIM+"/"+MVPROVIS+"/"+MVTAXA+"/"+MVINSS+"/"+MVISS+"/"+MVTXA +"/"+"SES"+"/"+MV_CPNEG+"/"+"INA") .Or.;
                   (AllTrim(M->E2_ORIGEM) $ "FINA290#FINA290M"  .And. Alltrim(M->E2_FATURA) == "NOTFAT") )

    //Limpa as variáveis de memória.
    If lTelaRet		//Consulta tipo de retenção
        If __lPccMR
            M->E2_PIS		:= 0
            M->E2_COFINS	:= 0
            M->E2_CSLL		:= 0
        Endif
        If __lIrfMR
            M->E2_IRRF		:= 0
        Endif
        If __lInsMR
            M->E2_INSS		:= 0
        Endif
        If __lIssMR
            M->E2_ISS		:= 0
        Endif
        If __lCidMR
            M->E2_CIDE		:= 0
        Endif
        If __lSestMR
            M->E2_SEST		:= 0
        Endif

        M->E2_VALOR := __nVlrMR
        aImpos := Aclone(__aVetImp)
    Else

        __lPccMR := .F.
        __lIrfMR := .F.
        __lInsMR := .F.
        __lIssMR := .F.
        __lCidMR := .F.
        __lSestMR := .F.
        __lOtImpMR := .F.

        If lAltera .And. cField == "M->E2_NATUREZ"
            cChaveFK7 := xFilial("SE2") + "|" + SE2->E2_PREFIXO + "|" + SE2->E2_NUM + "|" + SE2->E2_PARCELA + "|" + SE2->E2_TIPO + "|" + SE2->E2_FORNECE + "|" + SE2->E2_LOJA
            cIdOriFKA := FINGRVFK7("SE2", cChaveFK7)
            FKA->(DbSetOrder(3))

            If FKA->(MsSeek(xFilial("FKA") + "SE2" + cIdOriFKA))
                aImpConf := FinImpConf("1", cFilAnt, SE2->E2_FORNECE, SE2->E2_LOJA, SE2->E2_NATUREZ)

                For nZ := 1 To Len(aImpConf)
                    Do Case
                        Case aImpConf[nZ,1] $ "PIS|COF|CSL"
                            __lPccMR := .T.
                        Case aImpConf[nZ,1] == "IRF"
                            __lIrfMR := .T.
                        Case aImpConf[nZ,1] == "INSS"
                            __lInsMR := .T.
                        Case aImpConf[nZ,1] == "ISS"
                            __lIssMR := .T.
                        Case aImpConf[nZ,1] == "CIDE"
                            __lCidMR := .T.
                        Case aImpConf[nZ,1] == "SEST"
                            __lSestMR := .T.
                    EndCase
                Next nZ

                //Refaz a valor de inclusão
                M->E2_VALOR := FinBaseMR(M->E2_VALOR, .T., SE2->E2_FILIAL, M->E2_PREFIXO, M->E2_NUM, M->E2_PARCELA, M->E2_TIPO, M->E2_FORNECE,;
                                            M->E2_LOJA, __lPccMR, __lIrfMR, __lInsMR, __lIssMR, __lCidMR, __lSestMR, SE2->E2_MOEDA, SE2->E2_TXMOEDA, SE2->E2_EMISSAO)[1]

                M->E2_BASEPIS := IIf(M->E2_BASEPIS > 0 , M->E2_BASEPIS, M->E2_VALOR)
                M->E2_BASECOF := IIf(M->E2_BASECOF > 0 , M->E2_BASECOF, M->E2_VALOR)
                M->E2_BASECSL := IIf(M->E2_BASECSL > 0 , M->E2_BASECSL, M->E2_VALOR)
                M->E2_BASEIRF := IIf(M->E2_BASEIRF > 0 , M->E2_BASEIRF, M->E2_VALOR)
                M->E2_BASEINS := IIf(M->E2_BASEINS > 0 , M->E2_BASEINS, M->E2_VALOR)
                M->E2_BASEISS := IIf(M->E2_BASEISS > 0 , M->E2_BASEISS, M->E2_VALOR)
                __nVlrMR := M->E2_VALOR
                nValDig := M->E2_VALOR

                __lPccMR := .F.
                __lIrfMR := .F.
                __lInsMR := .F.
                __lIssMR := .F.
                __lCidMR := .F.
                __lSestMR := .F.
            EndIf
        ElseIf __nVlrMR > 0 .And. !(cField $ "M->E2_VALOR|M->E2_MOEDA|M->E2_ISS|M->E2_IRRF|M->E2_INSS|M->E2_SEST|M->E2_COFINS|M->E2_PIS|M->E2_CSLL|M->E2_BTRISS")
            M->E2_VALOR := __nVlrMR
            lRefazVlr := .T.
        EndIf

        //Cálculo dos impostos
        If lCalcImp .And. !Empty(M->E2_VALOR)
            nBaseMR := M->E2_VALOR

            If M->E2_MOEDA > 1
                nBaseMR := NoRound(xMoeda(M->E2_VALOR, M->E2_MOEDA, 1, M->E2_EMISSAO, (nCasDec + 1), M->E2_TXMOEDA), nCasDec)
            EndIf

            aImpos := FinCalImp("1", M->E2_NATUREZ, M->E2_FORNECE, M->E2_LOJA, cFilAnt, nBaseMR, dDataBase, .F., {}, M->E2_TIPO, cChaveFK7, Nil, {})
        EndIf

        If Len(aImpos) > 0

            M->E2_PIS		:= 0
            M->E2_COFINS	:= 0
            M->E2_CSLL		:= 0
            M->E2_IRRF		:= 0
            M->E2_INSS		:= 0
            M->E2_ISS		:= 0
            M->E2_CIDE		:= 0
            M->E2_SEST		:= 0

            M->E2_BASEPIS := M->E2_VALOR
            M->E2_BASECOF := M->E2_VALOR
            M->E2_BASECSL := M->E2_VALOR
            M->E2_BASEIRF := M->E2_VALOR
            M->E2_BASEINS := M->E2_VALOR
            M->E2_BASEISS := M->E2_VALOR

             //controla grv imp MR na alteração
             __lGrvMR := .T.
        EndIf

        //Salva o valor de inclusão.
        __nVlrMR := If(lF050Auto, nValDig, M->E2_VALOR)


    EndIf

    nImpos := Len(aImpos)

    For nZ := 1 To nImpos
        Do Case
            Case aImpos[nZ,8] $ "PIS"
                M->E2_PIS += aImpos[nZ,5]
                __lPccMR := .T.
                nPosPcc := nZ
                M->E2_BASEPIS := aImpos[nZ,2]
                __lPccBxMR := aImpos[nZ,9] == "2"
            Case aImpos[nZ,8] $ "COF"
                M->E2_COFINS += aImpos[nZ,5]
                __lPccMR := .T.
                nPosPcc := nZ
                M->E2_BASECOF := aImpos[nZ,2]
                __lPccBxMR := aImpos[nZ,9] == "2"
            Case aImpos[nZ,8] $ "CSL"
                M->E2_CSLL += aImpos[nZ,5]
                __lPccMR := .T.
                nPosPcc := nZ
                M->E2_BASECSL := aImpos[nZ,2]
                __lPccBxMR := aImpos[nZ,9] == "2"
            Case aImpos[nZ,8] == "IRF"
                M->E2_IRRF += aImpos[nZ,5]
                __lIrfMR := .T.
                nPosIrf := nZ
                M->E2_BASEIRF := aImpos[nZ,2]
                __lIrfBxMR := aImpos[nZ,9] == "2"
            Case aImpos[nZ,8] == "INSS"
                M->E2_INSS += aImpos[nZ,5]
                __lInsMR := .T.
                nPosIns := nZ
                M->E2_BASEINS := aImpos[nZ,2]
            Case aImpos[nZ,8] == "ISS"
                M->E2_ISS += aImpos[nZ,5]
                __lIssMR := .T.
                nPosIss := nZ
                M->E2_BASEISS := aImpos[nZ,2]
            Case aImpos[nZ,8] == "CIDE"
                M->E2_CIDE += aImpos[nZ,5]
                __lCidMR := .T.
                nPosCid := nZ
            Case aImpos[nZ,8] == "SEST"
                M->E2_SEST += aImpos[nZ,5]
                __lSestMR := .T.
                nPosSes := nZ
            OtherWise
                Aadd(aOutImp, nZ)
                __lOtImpMR := .T.
        EndCase
    Next nZ

    /*
    aImpos[z,9] = fator gerador competência ou caixa (PA)
    aImpos[z,13] = ação aplicadasobre o vlr da nf (1 = subtrair as retenções)
    aImpos[z,14] = carteira de movimento (1 = pagar)
    aImpos[z,15] = Tipo de mov na emissão (1 = Abtimento, 2 = Impostos)
    */
    //===Verifica quais impostos terão a retenção abatida do vlr da nota===//
    //PCC
    If lIsPA	//Titulos de adiantamento (PA)
        If __lPccMR
            If aImpos[nPosPcc,14] == "1"
                If M->E2_MOEDA > 1
                    nValorAux	:= Round(xMoeda(M->E2_PIS, 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                    M->E2_VALOR -= nValorAux
                    __nImpMR += nValorAux

                    nValorAux	:= Round(xMoeda(M->E2_COFINS, 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                    M->E2_VALOR -= nValorAux
                    __nImpMR += nValorAux

                    nValorAux 	:= Round(xMoeda(M->E2_CSLL, 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                    M->E2_VALOR -= nValorAux
                    __nImpMR += nValorAux
                Else
                    nValorAux	:= (M->E2_PIS+M->E2_COFINS+M->E2_CSLL)
				    M->E2_VALOR -= nValorAux
				    __nImpMR += nValorAux
                EndIf
            EndIf
        EndIf

        //IRF
        If __lIrfMR
            If aImpos[nPosIrf,14] == "1"
                If M->E2_MOEDA > 1
                    nValorAux 	:= Round(xMoeda(M->E2_IRRF, 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                Else
                    nValorAux	:= M->E2_IRRF
                EndIf
                M->E2_VALOR -= nValorAux
			    __nImpMR += nValorAux
            EndIf
        EndIf

        //INSS
        If __lInsMR
            If aImpos[nPosIns,14] == "1"
                If M->E2_MOEDA > 1
                    nValorAux	:= Round(xMoeda(M->E2_INSS, 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                Else
                    nValorAux	:= M->E2_INSS
                EndIf
                M->E2_VALOR -= nValorAux
			    __nImpMR += nValorAux
            EndIf
        EndIf

        //ISS
        If __lIssMR
            If aImpos[nPosIss,14] == "1"
                If M->E2_MOEDA > 1
                    nValorAux	:= Round(xMoeda(M->E2_ISS, 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                Else
                    nValorAux	:= M->E2_ISS
                EndIf
                M->E2_VALOR -= nValorAux
			    __nImpMR += nValorAux
            EndIf
        EndIf

        //Cide
        If __lCidMR
            If aImpos[nPosCid,14] == "1"
                If M->E2_MOEDA > 1
                    nValorAux	:= Round(xMoeda(M->E2_CIDE, 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                Else
                    nValorAux	:= M->E2_CIDE
                EndIf
                M->E2_VALOR -= nValorAux
			    __nImpMR += nValorAux
            EndIf
        EndIf

        //SEST
        If __lSestMR
            If aImpos[nPosSes,14] == "1"
                If M->E2_MOEDA > 1
                    nValorAux	:= Round(xMoeda(M->E2_SEST, 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                Else
                    nValorAux	:= M->E2_SEST
                EndIf
                M->E2_VALOR -= nValorAux
			    __nImpMR += nValorAux
            EndIf
        EndIf

        //Outros impostos
        If __lOtImpMR
            nImpos := Len(aOutImp)
            __nImpMR := 0

            For nZ := 1 To nImpos
                nY := aOutImp[nZ]

                If aImpos[nY,14] != "1"
                    Loop
                EndIf

                If M->E2_MOEDA > 1
                    M->E2_VALOR -= Round(xMoeda(aImpos[nY,5], 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                    __nImpMR += Round(xMoeda(aImpos[nY,5], 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                Else
                    M->E2_VALOR -= aImpos[nY,5]
                    __nImpMR += aImpos[nY,5]
                EndIf
            Next nZ
        EndIf

    Else		//Titulos normais
        If __lPccMR .And. aImpos[nPosPcc,9] == "1" .And. aImpos[nPosPcc,15] == "2"
            If aImpos[nPosPcc,13] == "1" .And. aImpos[nPosPcc,14] == "1"
                If M->E2_MOEDA > 1
                    nValorAux 	:= Round(xMoeda(M->E2_PIS, 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                    M->E2_VALOR -= nValorAux
                    __nImpMR += nValorAux

                    nValorAux	:= Round(xMoeda(M->E2_COFINS, 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                    M->E2_VALOR -= nValorAux
                    __nImpMR += nValorAux

                    nValorAux	:= Round(xMoeda(M->E2_CSLL, 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                    M->E2_VALOR -= nValorAux
                    __nImpMR += nValorAux
                Else
                    nValorAux	:= (M->E2_PIS+M->E2_COFINS+M->E2_CSLL)
				    M->E2_VALOR -= nValorAux
				    __nImpMR += nValorAux
                EndIf
            EndIf
        EndIf

        //IRF
        If __lIrfMR .And. aImpos[nPosIrf,9] == "1" .And. aImpos[nPosIrf,15] == "2"
            If aImpos[nPosIrf,13] == "1" .And. aImpos[nPosIrf,14] == "1"
                If M->E2_MOEDA > 1
                    nValorAux 	:= Round(xMoeda(M->E2_IRRF, 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                Else
                    nValorAux 	:= M->E2_IRRF
                EndIf
                M->E2_VALOR -= nValorAux
			    __nImpMR += nValorAux
            EndIf
        EndIf

        //INSS
        If __lInsMR .And. aImpos[nPosIns,9] == "1" .And. aImpos[nPosIns,15] == "2"
            If aImpos[nPosIns,13] == "1" .And. aImpos[nPosIns,14] == "1"
                If M->E2_MOEDA > 1
                    nValorAux 	:= Round(xMoeda(M->E2_INSS, 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                Else
                    nValorAux 	:= M->E2_INSS
                EndIf
                M->E2_VALOR -= nValorAux
			    __nImpMR += nValorAux
            EndIf
        EndIf

        //ISS
        If __lIssMR .And. aImpos[nPosIss,9] == "1" .And. aImpos[nPosIss,15] == "2"
            If aImpos[nPosIss,13] == "1" .And. aImpos[nPosIss,14] == "1"
                If M->E2_MOEDA > 1
                    nValorAux	:= Round(xMoeda(M->E2_ISS, 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                Else
                    nValorAux	:= M->E2_ISS
                EndIf
                M->E2_VALOR -= nValorAux
			    __nImpMR += nValorAux
            EndIf
        EndIf

        //Cide
        If __lCidMR .And. aImpos[nPosCid,9] == "1" .And. aImpos[nPosCid,15] == "2"
            If aImpos[nPosCid,13] == "1" .And. aImpos[nPosCid,14] == "1"
                If M->E2_MOEDA > 1
                    nValorAux	:= Round(xMoeda(M->E2_CIDE, 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                Else
                    nValorAux	:= M->E2_CIDE
                EndIf
                M->E2_VALOR -= nValorAux
			    __nImpMR += nValorAux
            EndIf
        EndIf

        //SEST
        If __lSestMR .And. aImpos[nPosSes,9] == "1" .And. aImpos[nPosSes,15] == "2"
            If aImpos[nPosSes,13] == "1" .And. aImpos[nPosSes,14] == "1"
                If M->E2_MOEDA > 1
                    nValorAux	:= Round(xMoeda(M->E2_SEST, 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                Else
                    nValorAux	:= M->E2_SEST
                EndIf
                M->E2_VALOR -= nValorAux
			    __nImpMR += nValorAux
            EndIf
        EndIf

        //Outros impostos
        If __lOtImpMR
            nImpos := Len(aOutImp)
            __nImpMR := 0

            For nZ := 1 To nImpos
                nY := aOutImp[nZ]

                If aImpos[nY,9] != "1" .Or. aImpos[nY,15] != "2" .Or. aImpos[nY,13] != "1" .Or. aImpos[nY,14] != "1"
                    Loop
                EndIf

                If M->E2_MOEDA > 1
                    M->E2_VALOR -= Round(xMoeda(aImpos[nY,5], 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                    __nImpMR += Round(xMoeda(aImpos[nY,5], 1, M->E2_MOEDA, M->E2_EMISSAO, (nCasDec + 1),, M->E2_TXMOEDA), 2)
                Else
                    M->E2_VALOR -= aImpos[nY,5]
                    __nImpMR += aImpos[nY,5]
                EndIf
            Next nZ
        EndIf
    EndIf

    __aVetImp := Aclone(aImpos)

Return Nil

//-------------------------------------------------------------------
/*/{Protheus.doc} F050ImpCon()

@author  Sivaldo Oliveira
@since 07/11/2017
@version 12
/*/
//-------------------------------------------------------------------
Static Function F050ImpCon(nOper As Numeric)

    Local nY As Numeric
    Local nImpConf As Numeric
    Local aImpConf As Array

    Default nOper := 0

    //Inicializa a variável
    nY := 0
    nImpConf := 0
    aImpConf := {}

    //Verifica quais os impostos configurados
    If nOper == 3 //Inclusao
        aImpConf := FinImpConf("1", cFilAnt, M->E2_FORNECE, M->E2_LOJA, M->E2_NATUREZ)
    Else
        aImpConf := FinImpConf("1", cFilAnt, SE2->E2_FORNECE, SE2->E2_LOJA, SE2->E2_NATUREZ)
    EndIf

    nImpConf := Len(aImpConf)

    For nY := 1 To nImpConf
        Do Case
            Case aImpConf[nY,1] $ "PIS|COF|CSL"
                __lPccMR := .T.
            Case aImpConf[nY,1] == "IRF"
                __lIrfMR := .T.
            Case aImpConf[nY,1] == "INSS"
                __lInsMR := .T.
            Case aImpConf[nY,1] == "ISS"
                __lIssMR := .T.
            Case aImpConf[nY,1] == "CIDE"
                __lCidMR := .T.
            Case aImpConf[nY,1] == "SEST"
                __lSestMR := .T.
            OtherWise
                __lOtImpMR := .T.
        EndCase
    Next nY

Return Nil

//-------------------------------------------------------
/*/{Protheus.doc} F050MRET
Chamada da tela de manutenção das retenções do motor

@author Mauricio Pequim Jr

@since 07/12/2017
@version P12
/*/
//-------------------------------------------------------
Function F050MRET()
	FINMRET(__aVetImp, 'SE2', .F., /*@nRetMotor*/)
	F050VldImp(.T.)
Return

//-------------------------------------------------------
/*/{Protheus.doc} F050BtrISS
	Valida se o codigo do servico foi preenchido,
	caso houver ISS

@author Igor Sousa do Nascimento

@since 23/10/2018
@version P12
/*/
//-------------------------------------------------------
Function F050BtrISS()
	Local aAreaSED := {}
	Local aAreaSA2 := {}
	Local aAreaCC2 := {}
	Local lRet := .T.

    Default __lFnBtr   := FindFunction("ISSCPOM") .And. FindFunction("BtrISSMun")
    Default __lBtrISS  := SE2->(ColumnPos("E2_BTRISS")) > 0 .And. SE2->(ColumnPos("E2_VRETBIS")) > 0 .And. SE2->(ColumnPos("E2_CODSERV")) > 0 .And. __lFnBtr

	If !IsBlind() .And. __lBtrISS .And. Type("M->E2_CODSERV") <> "U"
		aAreaSED := SED->( GetArea() )
		SED->( dbSetOrder(1) ) //ED_FILIAL+ED_CODIGO
		If SED->( msSeek( FWxFilial("SED") + M->E2_NATUREZ ) )
			If SED->ED_CALCISS == "S" .And. Empty( AllTrim(M->E2_CODSERV) )

				aAreaSA2 := SA2->( GetArea() )
				SA2->( dbSetOrder(1) ) //A2_FILIAL+A2_COD+A2_LOJA
				If SA2->( msSeek( FWxFilial("SA2") + M->E2_FORNECE + M->E2_LOJA ) )
					If Upper( AllTrim(SA2->A2_MUN) ) <> Upper( AllTrim(SM0->M0_CIDENT) )

						aAreaCC2 := CC2->( GetArea() )
						CC2->( dbSetOrder(2) ) //CC2_FILIAL+CC2_MUN
						If CC2->( msSeek( FWxFilial("CC2") + Pad( Upper(SM0->M0_CIDENT), TamSX3("CC2_MUN")[1] ) ) )
							If MsgYesNo(STR0319) //"Titulo com ISS: Municipio exige CPOM?"
								lRet := .F.
								Help("", 1, "F050CODSER")
							EndIf
						EndIf

						RestArea(aAreaCC2)
						FwFreeArray(aAreaCC2)
					EndIf
				EndIf

				RestArea(aAreaSA2)
				FwFreeArray(aAreaSA2)
			EndIf
		EndIf

		RestArea(aAreaSED)
		FwFreeArray(aAreaSED)
	EndIf

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} F050Restr
Montagem dos campos para a MarkBrowse
@author Mauricio Pequim Jr

@version P12
@since  06/08/2019
@return  Nil
/*/
//-------------------------------------------------------------------
Function F050Restr() AS ARRAY

    Local aRestrict AS ARRAY

    aRestrict := __aRestric
    
    __aPesqui	:= {}
    __aCbx		:= {}

    IF Len(__aIndices) == 0
        //Tela de indices para seleção
	    FinCposSix("SE2",@__aIndices,@aRestrict)
        //Verifica se existem campos de usuario
        FSubsCpoU(@aRestrict, .T.)
    Endif

Return aRestrict

//-----------------------------------------------------------------------------------------------------------------------
/*/{Protheus.doc} f050QryA
Montagem do Temporário

@author Mauricio Pequim Jr
@version P12
@since  06/08/2019
@return  nIl
/*/
//-----------------------------------------------------------------------------------------------------------------------
Static Function f050QryA(cAliasSE2 AS Character, aCampos AS Array, aRestrict AS Array,;
                            aSelFil AS Array, aTmpFil AS Array, cOutMoeda AS Character,;
                            nMoedSubs AS Numeric) AS Character

	Local cQuery        AS Character
	Local cFiltro       AS Character
	//--- Tratamento Gestao Corporativa
	Local cLayout       AS Character
    Local lGestao	    AS Logical
	Local cFilFwSE2     AS Character
	Local cTmpSE2Fil    AS Character

    cQuery          := ""
    cFiltro         := ""
    cLayout         := FWSM0Layout()
    lGestao	        := "E" $ cLayout .Or. "U" $ cLayout
    cFilFwSE2       := IIF( lGestao , FwFilial("SE2") , xFilial("SE2") )
    cTmpSE2Fil      := ""

	DEFAULT cAliasSE2   := ""
	DEFAULT aCampos     := {}
	DEFAULT aRestrict   := {}
	DEFAULT aSelFil     := {}
	DEFAULT aTmpFil     := {}
	DEFAULT cOutMoeda   := "1"
	DEFAULT nMoedSubs   := 1

    dbSelectArea("SE2")

	If Select("__SUBS") > 0
        TcSQLExec("DELETE FROM "+__cFIN2Name)
        __SUBS->(DBGOTO(1))
	EndIf

	dbSelectArea("SE2")

	cFiltro := FA050Chec2(cOutMoeda, nMoedSubs)

	cQuery := "SELECT "
	aEval(aRestrict,{|x| cQuery += x + ", "})
	cQuery += "E2_OK, R_E_C_N_O_ NUM_REG "
	cQuery += "FROM " + RetSqlName("SE2") + " "
	cQuery += "WHERE "

	// Contas a pagar compartilhado deve observar FILORIG para realizar filtro
	If Empty( cFilFwSE2 )
	    cQuery += "E2_FILORIG " + GetRngFil( aSelFil, "SE2", .T., @cTmpSE2Fil, , .T. ) + " AND "
	Else
	    cQuery += "E2_FILIAL " + GetRngFil( aSelFil, "SE2", .T., @cTmpSE2Fil ) + " AND "
	EndIf
	aAdd(aTMPFil, cTmpSE2Fil)
	cQuery += cFiltro + " "
	cQuery += " AND D_E_L_E_T_ = ' ' "
	cQuery += " ORDER BY " + SqlOrder(SE2->(IndexKey()))
	cQuery := ChangeQuery(cQuery)

	cArqNew := F050MTTMP(cQuery,aCampos,aRestrict,cAliasSE2)

	dbSelectArea(cArqNew)
	(cArqNew)->(DbGoTop())

Return cArqNew

//-------------------------------------------------------------------
/*/{Protheus.doc} FA050Chec2

Faz a query de filtro dos titulos

@author Mauricio Pequim Jr
@since  06/08/2019
@version P11
/*/
//-------------------------------------------------------------------
Static Function FA050Chec2(cOutMoeda, nMoedSubs)
    Local cQuery := ""
    Local cTipoProvis := ""
    Local cOrigens   := F050TipoIN("CNTA090|CNTA100|CNTA120|FINA171|FINI055",.T.,"E2_ORIGEM")

    If cPaisLoc == "EQU"
        cTipoProvis := MVPROVIS
        cTipoProvis += "|NF "
    Else
        cTipoProvis := MVPROVIS
    EndIf

    cQuery += " E2_FORNECE = '"+ cCodFor + "' "
    cQuery += " AND E2_LOJA = '" + cLojaFor + "' "
    cQuery += " AND E2_SALDO = E2_VALOR "
    cQuery += " AND E2_TIPO IN " + FORMATIN(cTipoProvis,"|")
    cQuery += " AND E2_EMISSAO <= '" + DTOS(dDatabase) + "' "	    //titulos com emissao futura serao desconsiderados
    cQuery += " AND E2_ORIGEM NOT IN " + cOrigens + " "

    If cOutMoeda == "1" // Nao considera outras moedas
        cQuery +=  ' AND E2_MOEDA = ' + Alltrim(STR(nMoedSubs))
    Endif

    // Complemento de filtro Siafi
    cQuery += FinTemDH(.T. /*lFiltro*/,/*cAlias*/,.F. /*lHelp*/, .T./*lTop*/)

    // Execblock para incluir filtragem na IndRegua
    // Devera retornar uma string no formato SQL para ser incluida na condicao.
    If ExistBlock( "FA050Fil" )
        cQuery += ExecBlock("FA050Fil",.f.,.f.)
    Endif

Return cQuery

//-----------------------------------------------------------------------------------------------------------------------
/*/{Protheus.doc} F050MTTMP
Montagem do Temporário

@author Mauricio Pequim Jr
@version P12
@since  06/08/2019
@return  nIl
/*/
//-----------------------------------------------------------------------------------------------------------------------
Function F050MTTMP(cQuery AS Character, aCampos AS Array, aRestrict AS Array, cAliasSE2 AS Character) AS Character
	Local aEstruct      AS Array
	Local aArea         AS Array
	Local nTcSql	    AS Numeric
	Local cFilFwSE2     AS Character
	Local cQuery2	    AS Character
	Local cCampos	    AS Character
	Local aSeek		    AS Array
    Local nStatus       AS Numeric
    Local aFields       AS Array
    Local cTipoSX3      AS Character
    Local cNivelSX3     AS Character
    Local cPropriSX3    AS Character
    Local cConteSX3     AS Character
    Local cPicturSX3    AS Character
    Local cTituSX3      AS Character
    Local cTamSX3       AS Character
    Local cDeciSX3      AS Character
    Local nX            AS Numeric
    Local nY            AS Numeric
    Local cCampoSX3     AS CHARACTER

    aFields         := FWSX3Util():GetAllFields("SE2")
    nStatus         := 0
    aSeek		    := {}
    cCampos         := ''
    cQuery2	        := ''
    cFilFwSE2       := xFilial("SE2")
    aEstruct        := {}
    nX              := 0
    aArea           := GetArea()
    nTcSql	        := 0

    // Montagem dos campos na Array
    aCampos := {}
    AADD(aCampos,{"E2_OK","","  ",""})

    For nY := 1 to Len(aFields)
        cCampoSX3   := Alltrim(upper(aFields[nY]))
        cTipoSX3    := GETSX3CACHE(aFields[nY], "X3_TIPO")
        cNivelSX3   := GETSX3CACHE(aFields[nY], "X3_NIVEL")
        cPropriSX3  := GETSX3CACHE(aFields[nY], "X3_PROPRI")
        cConteSX3   := GETSX3CACHE(aFields[nY], "X3_CONTEXT")
        cPicturSX3  := GETSX3CACHE(aFields[nY], "X3_PICTURE")
        cTamSX3     := GETSX3CACHE(aFields[nY], "X3_TAMANHO")
        cDeciSX3    := GETSX3CACHE(aFields[nY], "X3_DECIMAL")
        cTituSX3    := Alltrim(FWX3Titulo((aFields[nY])))

        //Adiciona o campo E2_FILIAL no browse somente se o SE2 estiver exclusivo e em uso.
        if cCampoSX3 == "E2_FILIAL"
            If !Empty( cFilFwSE2 ) .Or. ;
                (X3USO(GETSX3CACHE(cCampoSX3, "X3_USADO")) .And. ;
                cNivel >= GETSX3CACHE(cCampoSX3, "X3_NIVEL") .AND. ;
                ASCAN(aRestrict,{|x| alltrim(upper(x)) == alltrim(upper(cCampoSX3))}) > 0)

                AAdd(aCampos,{cCampoSX3,"",AllTrim(FWX3Titulo(cCampoSX3)),GETSX3CACHE(cCampoSX3, "X3_PICTURE")})
                AAdd( aEstruct,	{ cCampoSX3, cTipoSX3, cTamSX3, cDeciSX3 } )

                Loop
            EndIf
        Endif

        IF ( ( ASCAN(aRestrict,{|x| Alltrim(upper(x)) == cCampoSX3 }) > 0) .AND. ;
            ( (cTipoSX3 != "M" .AND. (cNivel >= cNivelSX3 .or. cPropriSX3 == "L") .and. cConteSX3 != "V") ) )

            If cNivel >= cNivelSX3 
                AAdd( aCampos, { cCampoSX3, "", cTituSX3, cPicturSX3 } )
            Endif
            
            AAdd( aEstruct,	{ cCampoSX3, cTipoSX3, cTamSX3, cDeciSX3 } )
        EndIf
    Next nY

    If aScan(aEstruct, {|x| Alltrim(UPPER(x[1])) == Alltrim(UPPER("E2_OK"))}) == 0
        AADD(aEstruct,{"E2_OK","C",2,0})
    Endif

    If aScan(aEstruct, {|x| Alltrim(UPPER(x[1])) == Alltrim(UPPER("NUM_REG"))}) == 0
        AADD(aEstruct,{"NUM_REG","N",10,0})
    Endif

	// Deleta o conteúdo da tabela temporária no banco de dados
	If __oFIN0502 <> Nil
	    nTcSql := TCSQLEXEC("DELETE FROM "+__cFIN2Name)
        FChkTCExec(nTcSql, 2)
	Endif

    // Criação da Tabela Temporaria ---------------------------
    if __oFIN0502 == Nil    
        __oFIN0502 := FWTemporaryTable():New( cAliasSE2 )
        __oFIN0502:SetFields( aEstruct )

        __nOrdOk := Len(__aIndices)
        //Adiciono o índice da tabela temporária
        For nX := 1 To __nOrdOK
            Aadd(aSeek,{__aIndices[nX,1],__aIndices[nX,2],nX})

            cTmpIdx := "Tmp_Idx_"+StrZero((nX+1),2)
            aChave	:= StrToKarr(Alltrim(__aIndices[nX,3]),"+")

            __oFIN0502:AddIndex(cTmpIdx,aChave)
        Next

        __nOrdOk += 1
        cTmpIdx := "Tmp_Idx_"+StrZero((nX+1),2)
        __oFIN0502:AddIndex(cTmpIdx,{"E2_OK"})

        __oFIN0502:Create()

        __cFIN2Name := __oFIN0502:GetRealName()  
    EndIf


	cQuery2 := " INSERT "
	If AllTrim(TcGetDb()) == "ORACLE"
	    cQuery2 += " /*+ APPEND */ "
	EndIf

	AEval( aRestrict, { |e,i| cCampos += If( i == 1, AllTrim(e), "," + AllTrim(e) ) } )

	cCampos += ',E2_OK, NUM_REG'

	If AllTrim(TcGetDb()) == "DB2"
	    cQuery := STRTRAN( cQuery, "FOR READ ONLY", "" )
	EndIf

	cQuery2 += " INTO " + __cFIN2Name + " (" + cCampos + " ) " + cQuery

	Processa( { || nTcSql := TcSQLExec(cQuery2) } )

	If nTcSql < 0
      If ExistFunc("FinxMsgE")
            FinxMsgE(TCSQLError())            
        Else
		    Help( " ", 1, "F050MTTMP", , STR0326 + CRLF +  CRLF + TCSQLError() , 1, 0 ) //"Não foi possivel montar a tabela temporaria, favor verificar o seu ambiente Protheus."        
       EndIf       		
	Endif

	(cAliasSE2)->(DbGoTop())

	RestArea(aArea)

Return cAliasSE2


//-------------------------------------------------------------------
/*/{Protheus.doc} Fa050Pesq
Pesquisa no arquivo temprário
@author Mauricio Pequim Jr

@version P12
@since  06/08/2019
@return  Nil
@obs Função utilizada nas rotinas FINA240 e FINA241
/*/
//-------------------------------------------------------------------
Function Fa050Pesq(oMark As Object, cAliasSE2 As Character, nIndice As Numeric)

	Local cCampo	As Character
	Local cSeek 	As Character 
	Local nX 		As Numeric
    
    nX      := 0
    cCampo	:= ""
    cSeek   := ""
    
    If Select(cAliasSE2) > 0

        If (cAliasSE2)->( !BOF() ) .and. (cAliasSE2)->( !EOF() )
            cCampo	:= (cAliasSE2)->(IndexKey())
            cSeek := IIF("_FILIAL" $ cCampo, xFilial("SE2"), "")

            If Len(__aPesqui) == 0 .and. Len(__aIndices) > 0
                For nX := 1 to Len(__aIndices)
                    cDescInd := Alltrim(__aIndices[nX,1])
                    aAdd(__aPesqui,{cDescInd ,nX})
                    aAdd(__aCbx,cDescInd)
                Next
            Endif

            dbSelectArea(cAliasSE2)

            nRet := Fa050Psq(oMark:oBrowse,__aPesqui,cSeek)
            oMark:oBrowse:Refresh(.T.)
        EndIf
        
    EndIf    

Return Nil

//-------------------------------------------------------------------
/*/{Protheus.doc} Fa050Psq
Pesquina no arquivo temprário
@author Mauricio Pequim Jr

@version P12
@since  06/08/2019
@return  Nil

/*/
//-------------------------------------------------------------------
Function Fa050Psq(oBrowse,__aPesqui,cSeek)

	Local oDlg		:= NIL
	Local oCbx		:= NIL
	Local cCampo	:= SPACE(100)
	Local cOrd		:= ""
	Local lSeek		:= .F.
	Local nRet		:= 0
	Local cAlias	:= Alias()
	Local nRecOri 	:= (cAlias)->(Recno())

	DEFINE MSDIALOG oDlg FROM 00,00 TO 110,550 PIXEL TITLE STR0001 //"Pesquisar"

	@ 05,05 COMBOBOX oCBX VAR cOrd ITEMS __aCbx SIZE 236,15 PIXEL OF oDlg FONT oDlg:oFont
	@ 22,05 MSGET oBigGet VAR cCampo SIZE 236,12 PIXEL

	DEFINE SBUTTON FROM 05,245 TYPE 1 OF oDlg ENABLE ACTION (nRet := 1,lSeek := .T., oDlg:End() )
	DEFINE SBUTTON FROM 20,245 TYPE 2 OF oDlg ENABLE ACTION oDlg:End()

	ACTIVATE MSDIALOG oDlg CENTERED

	If lSeek
		dbSetOrder(__aPesqui[aScan(__aCbx,Alltrim(cOrd))][2])

		If dbSeek(cSeek+RTRIM(cCampo))
			nRet := Recno()
		Else
			nRet := nRecOri
		EndIf

		If oBrowse != Nil
			oBrowse:Refresh()
		EndIf
	EndIf

Return nRet

//-------------------------------------------------------------------
/*/{Protheus.doc} F050TipoIN
Monta a expressão do IN da query

@author Mauricio Pequim Jr
@since  06/08/2019
@version 12.1.7
/*/
//-------------------------------------------------------------------
Function F050TipoIN(cTipos, lQryIn, cCampo)

    Local nTamCpo := 0
    Local aCampos := {}
    Local nX := 0

    Default cTipos := MV_CPNEG +"|"+ MVPAGANT +"|"+ MVISS +"|"+ MVTAXA +"|"+ MVTXA +"|"+ MVINSS +"|"+ 'SES' +"|"+ 'CID' + "|"+ 'INA' + "|"+ MVPROVIS
    Default lQryIn := .T.
    Default cCampo := ""

    cTipos	:=	StrTran(cTipos,',','/')
    cTipos	:=	StrTran(cTipos,';','/')
    cTipos	:=	StrTran(cTipos,'|','/')
    cTipos	:=	StrTran(cTipos,'\','/')

    //Quando quero montar uma expressão em que cada item da string tenha o tamanho de um determinado campo
    If !Empty(cCampo)
        nTamCpo := TamSx3(cCampo)[1]
        aCampos := Strtokarr2( cTipos, "/", .F.)
        cTipos := ""
        nLenCpos := Len(aCampos)
        For nX := 1 to nLenCpos
            cTipos += PadR(aCampos[nX],nTamCpo)
            If nX < nLenCpos
                cTipos += "/"
            Endif
        Next
    Endif

    If lQryIn
        cTipos := Formatin(cTipos,"/")
    Endif

Return cTipos


//-----------------------------------------------------------------------------------------------
/*/{Protheus.doc} FiltDtINSS
Função para filtrar o E2_EMISSAO ou E2_VENCREA nas queries de verificação
de cumulatividade do INSS

@author pedro.alencar
@since 19/07/2019
@version 12.1.28
@type function

@param dEmissao, date, Data de emissão do título financeiro
@param dVencrea, date, Data de vencimento real do título financeiro
@return cRet, char, Complemento da cláusula WHERE, filtrando o campo E2_EMISSAO ou E2_VENCREA

@obs Cumulatividade para fornecedores pessoa fisica ou juridica:
     http://tdn.totvs.com/pages/viewpage.action?pageId=271413168
/*/
//-----------------------------------------------------------------------------------------------
Static Function FiltDtINSS( dEmissao As Date, dVencRea As Date ) As Char
    Local cRet As Char
    Local cTpCumulat As Char
    Local dDataIni As Date
    Local dDataFim As Date
    Local lVencto As Logical
    Default dEmissao := CTOD("//")
    Default dVencRea := CTOD("//")

    cRet := ""
    cTpCumulat := SuperGetMv( "MV_INSTPAC", .F., "1" ) //Default "1"
    lVencto := SuperGetMv( "MV_ACMINSS", .T., "1" ) == "2" //1 = Emissao  2= Vencimento Real

	If cTpCumulat == "2" //MV_INSTPAC = '2' - Cumulativo por cinco anos
	    dDataIni := CTOD( '01/' + STR( MONTH(dEmissao) ) + '/' + STR( YEAR(dEmissao) - 5 ) )
	    dDataFim := LastDay(dEmissao)

	    cRet := "E2_EMISSAO BETWEEN '" + Dtos(dDataIni) + "' AND '" + Dtos(dDataFim)+ "' AND "
	Else //MV_INSTPAC = '1' - Cumulativo mensal
	    If lVencto .And. !Empty(dVencRea)
	        cRet := "E2_VENCREA BETWEEN '" + Dtos(FirstDay(dVencRea)) + "' AND '" + Dtos(LastDay(dVencRea)) + "' AND "
	    ElseIf !Empty(dEmissao)
	        cRet := "E2_EMISSAO BETWEEN '" + Dtos(FirstDay(dEmissao)) + "' AND '" + Dtos(LastDay(dEmissao)) + "' AND "
	    Endif
	EndIf

Return cRet


//-------------------------------------------------------------------
/*/{Protheus.doc} F050GrvAFR
Função para gravação da tabela AFR (Projeto x Despesas Financeiras)
durante o processo de Substituição

@author Mauricio Pequim Jr
@since 18/07/2019
@version 12.1.28
@return Nil
/*/
//-------------------------------------------------------------------

Function F050GrvAFR(aGravaAFR,aNtit,nMaxTam)

    DEFAULT aGravaAFR := {}
    DEFAULT aNtit := {}
    DEFAULT nMaxTam := 0

    If Len(aGravaAFR) > 0 .and. Len(aNtit) > 0 .and. nMaxTam > 0
        RecLock("AFR",.T.)
        AFR->AFR_FILIAL	:= aGravaAFR[1]
        AFR->AFR_PROJET	:= aGravaAFR[2]
        AFR->AFR_REVISA	:= aGravaAFR[3]
        AFR->AFR_TAREFA	:= aGravaAFR[4]
        AFR->AFR_TIPOD	:= aGravaAFR[17]
        AFR->AFR_PREFIX	:= aNtit[nMaxTam][1]
        AFR->AFR_NUM	:= aNtit[nMaxTam][2]
        AFR->AFR_PARCEL	:= aNtit[nMaxTam][3]
        AFR->AFR_TIPO	:= aNtit[nMaxTam][4]
        AFR->AFR_FORNEC	:= aNtit[nMaxTam][5]
        AFR->AFR_LOJA	:= aNtit[nMaxTam][6]
        AFR->AFR_VENREA	:= aNtit[nMaxTam][7]
        AFR->AFR_VALOR1	:= aNtit[nMaxTam][8]
        AFR->AFR_VALOR2	:= aNtit[nMaxTam][9]
        AFR->AFR_VALOR3	:= aNtit[nMaxTam][10]
        AFR->AFR_VALOR4	:= aNtit[nMaxTam][11]
        AFR->AFR_VALOR5	:= aNtit[nMaxTam][12]

        aNtit[nMaxTam][14]:= .T.

        AFR->( MsUnLock() )
    Endif
Return

//-------------------------------------------------------------------
/*/{Protheus.doc} F050VlAbt
Funcao que valida o valor do abatimento a ser gerado ao título
Posiciona a SE2 no Titulo Selecionado quando houver
mais de um titulo de abatimento

@author Luiz Henrique
@since 05/08/2019
@version 12.1.25
@return lRet - Valor válido(OK)
/*/
//-------------------------------------------------------------------
Function F050VlAbt() As Logical
    Local lRet 		As Logical
    Local lTemPai 	As Logical
    Local nValTot 	As Numeric
    Local nValTit 	As Numeric
    Local aAreaSE2 	As Array
    Local aSelTit	As Array
    Local nLinSel	As Numeric
    Local cChaveSE2 As Character
    Local cChavePai As Character

    lRet 		:= .T.
    lTemPai 	:= .F.
    nValTot 	:= 0
    nValTit 	:= 0
    aAreaSE2 	:= {}
    aSelTit	    := {}
    nLinSel	    := 0
    cChaveSE2   := ""
    cChavePai   := ""

    aAreaSE2 := SE2->( GetArea() )

    SE2->( dbSetOrder(1) ) // E2_FILIAL + E2_PREFIXO + E2_NUM + E2_PARCELA + E2_TIPO + E2_FORNECE + E2_LOJA
    
    If EMPTY(M->E2_PREFIXO + M->E2_NUM + M->E2_PARCELA) .and. M->E2_TIPO $ MVABATIM
        M->E2_NUM       := SE2->E2_NUM
        M->E2_PREFIXO   := SE2->E2_PREFIXO
        M->E2_PARCELA   := SE2->E2_PARCELA
    EndIf
    
    cChaveSE2 := FWxFilial("SE2") + M->E2_PREFIXO + M->E2_NUM + M->E2_PARCELA
    If !SE2->( dbSeek(cChaveSE2) )
        Help( ,, "F050VLABTTIT",, STR0336, 1, 0,,,,,, {STR0337} ) //"Não foi econtrado nenhum título pai para vincular o abatimento." ## "Digite uma chave valida."
        lRet := .F.
    Else
        While SE2->( !Eof() ) .And. SE2->(E2_FILIAL + E2_PREFIXO + E2_NUM + E2_PARCELA) == cChaveSE2 //soma dos abatimentos
            cChavePai := SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)
            If SE2->E2_TIPO $ MVABATIM+"/"+ MVPAGANT+"/"+MVPROVIS+"/"+MV_CPNEG 
                If lAltera
                    If SE2->E2_TIPO $ MVABATIM // Para alteração do abatimento o valor é o digitado na tela
                        nValTot += M->E2_VALOR
                    Else
                        nValTot += SE2->E2_VALOR
                    EndIf
                Else
                    nValTot += SE2->E2_SALDO
                EndIf
            ElseIf !lAltera .And. SE2->E2_SALDO > 0 .And. SE2->E2_SALDO >= M->E2_SALDO .and. !FTemAbat(cChavePai,M->E2_TIPO) .And. !SE2->E2_TIPO $ MVPAGANT+"/"+MVPROVIS+"/"+MV_CPNEG  
                AADD( aSelTit, {SE2->E2_PREFIXO, SE2->E2_NUM, SE2->E2_PARCELA ,SE2->E2_TIPO, SE2->E2_VALOR, SE2->E2_SALDO,SE2->E2_FORNECE,SE2->E2_LOJA} )
            Endif

            SE2->( dbSkip() )
        EndDo

        If lAltera .And. SE2->( dbSeek(xFilial("SE2") + M->E2_TITPAI) ) //posiciono no título pai
            nValTit := SE2->E2_VALOR
            lTemPai := .T.
        EndIf

        If Len(aSelTit) > 1

            DEFINE MSDIALOG oDlg FROM 6, 6 TO 30, 80 TITLE STR0327 // "Títulos para abatimento."

            @ 0.5, 1.5	SAY STR0328 //Localizamos mais de um título com a mesma chave.
            @ 1.2, 1.5	SAY STR0329 //Selecione um dos títulos para a amarração do abatimento.
            @ 2.4, 1.5 LISTBOX oLstBox FIELDS HEADER 	AllTrim(FwX3Titulo('E2_PREFIXO')),;
                                                        AllTrim(FwX3Titulo('E2_NUM')),;
                                                        AllTrim(FwX3Titulo('E2_PARCELA')),;
                                                        AllTrim(FwX3Titulo('E2_TIPO')),;
                                                        AllTrim(FwX3Titulo('E2_VALOR')),;
                                                        AllTrim(FwX3Titulo('E2_SALDO')),;
                                                        AllTrim(FwX3Titulo('E2_FORNECE')),;
                                                        AllTrim(FwX3Titulo('E2_LOJA')) ;
            SIZE 270 , 105 Font oDlg:oFont

            oLstBox:SetArray(aSelTit)
            oLstBox:bLine := { || { aSelTit[oLstBox:nAt,01], aSelTit[oLstBox:nAt,02], aSelTit[oLstBox:nAt,03], aSelTit[oLstBox:nAt,04], aSelTit[oLstBox:nAt,05], aSelTit[oLstBox:nAt,06], aSelTit[oLstBox:nAt,07], aSelTit[oLstBox:nAt,08] } }

            DEFINE SBUTTON FROM 150,215 TYPE 1 ACTION (nLinSel := oLstBox:nAt,oDlg:End()) ENABLE OF oDlg
            DEFINE SBUTTON FROM 150,245 TYPE 2 ACTION oDlg:End() ENABLE OF oDlg

            ACTIVATE MSDIALOG oDlg CENTERED

            If nLinSel == 0
                lRet := .F.
                Help( ,, "F050VLABTNOSEL",, STR0334, 1, 0,,,,,, {STR0335} ) //"Não foi selecionado nenhum título pai para vincular o abatimento." ## "Clique em 'Salvat' e selecione um título pai para vincular o abatimento."
            Else
                nValTit := aSelTit[nLinSel][6]
                If SE2->( dbSeek(xFilial("SE2") + aSelTit[nLinSel][1] + aSelTit[nLinSel][2] + aSelTit[nLinSel][3] + aSelTit[nLinSel][4] + aSelTit[nLinSel][7] + aSelTit[nLinSel][8] ) )	//posiciono no título selecionado
                    FA050Herda()
                    cTitPaiAB := SE2->(E2_PREFIXO + E2_NUM + E2_PARCELA + E2_TIPO + E2_FORNECE + E2_LOJA) //atualizo o titpai com o título selecionado
                    cTipoParaAbater := SE2->E2_TIPO
                    __SelPai := .T.
                    aAreaSE2 := SE2->( GetArea() ) //ajusto a area da SE2 para o título selecionado
                EndIF
            EndIf

        ElseIf Len(aSelTit) == 1
            nValTit := aSelTit[1][6]
            If SE2->( dbSeek(xFilial("SE2") + aSelTit[1][1] + aSelTit[1][2] + aSelTit[1][3] + aSelTit[1][4] + aSelTit[1][7] + aSelTit[1][8] ) ) //posiciono no título selecionado
                FA050Herda()
                cTitPaiAB := SE2->(E2_PREFIXO + E2_NUM + E2_PARCELA + E2_TIPO + E2_FORNECE + E2_LOJA) //atualizo o titpai com o título selecionado
                cTipoParaAbater := SE2->E2_TIPO
                __SelPai := .T.
                aAreaSE2 := SE2->( GetArea() ) //ajusto a area da SE2 para o título selecionado
                nRegistro := SE2->( RECNO() )
            EndIf
        ElseIf !lTemPai .and. M->E2_TIPO $ MVABATIM
            Help( ,, "F050VLABTTIT",, STR0336, 1, 0,,,,,, {STR0337} ) //"Não foi econtrado nenhum título pai para vincular o abatimento." ## "Digite uma chave valida."
            lRet := .F.
        EndIf

        nValTot := IIF(lAltera, nValTot, (M->E2_VALOR + nValTot))

        If lRet .And. lTemPai .And. nValTot > nValTit
            Help( ,, "F050VLABT",, STR0338, 1, 0,,,,,, { STR0339 + cValToChar(nValTit) } ) //"O valor informado para o abatimento é maior do que o valor do título." ## "Informe um valor menor ou igual ao valor do título: "
            lRet := .F.
        Endif

    Endif

    If SE2->( dbSeek(xFilial("SE2")+M->(E2_NUM+E2_PREFIXO+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)) )
        Help(" ",1,"FA050NUM")
        M->E2_NUM     := CRIAVAR("E2_NUM")
        lRet := .F.
    Endif

    RestArea(aAreaSE2)
    FwFreeArray(aAreaSE2)

    If __oAbtQry <> Nil
		__oAbtQry:Destroy()
		__oAbtQry := Nil
	Endif

Return lRet

//-----------------------------------------------------------------------------------------------
/*/{Protheus.doc} RecompoeVl
Função para recompor o valor inicial do título na alteração, caso a natureza
seja alterada

@author pedro.alencar
@since 22/08/2019
@version 12.1.28
@type function
@return Nil
/*/
//-----------------------------------------------------------------------------------------------
Function RecompoeVl(lIRPFBaixa AS Logical, lCalcIssBx AS Logical, lPCCBaixa AS Logical)
	Local nTotImp As Numeric
	Local nMoeda As Numeric

    DEFAULT lIRPFBaixa := .F.
    DEFAULT lCalcIssBx := .F.
    DEFAULT lPCCBaixa := .F.
    DEFAULT __lFnBtr   := FindFunction("ISSCPOM") .And. FindFunction("BtrISSMun")
    Default __lBtrISS  := SE2->(ColumnPos("E2_BTRISS")) > 0 .And. SE2->(ColumnPos("E2_VRETBIS")) > 0 .And. SE2->(ColumnPos("E2_CODSERV")) > 0 .And. __lFnBtr

	If __nVlrMR > 0
		nValDig := __nVlrMR
	Else
		nTotImp := M->( E2_INSS + E2_PRINSS + E2_SEST )
		nTotImp += Iif(lIRPFBaixa, 0, M->E2_IRRF )
		nTotImp += Iif(lCalcIssBx, 0, M->(E2_ISS + E2_PRISS) )
		nTotImp += Iif(lPCCBaixa, 0, M->(E2_PIS + E2_COFINS + E2_CSLL) )
	    nTotImp += Iif( __lBtrISS, M->E2_BTRISS, 0 )

		//Converte os impostos para a moeda do título, para somar corretamente
		nMoeda := M->E2_MOEDA
		If nTotImp > 0 .And. nMoeda > 1
			nTotImp := xMoeda( nTotImp, 1, nMoeda, M->E2_EMISSAO, MsDecimais(1),, M->E2_TXMOEDA )
		EndIf

		//Define o valor da private com o último valor bruto "digitado" do título
		nValDig := M->E2_VALOR + nTotImp
	EndIf

	M->E2_VALOR := nValDig
Return Nil

//-------------------------------------------------------
/*/ F050CodRet

@author Simone Mie Sato Kakinoana
@since 29/10/2019
@version P12
Valida‡?o para o Codigo de reten‡?o da DIRF
*/
//-------------------------------------------------------
Function F050CodRet()

    Local lRet	:= .T.

    if ( M->E2_DIRF == "1" ) .And. Empty( M->E2_CODRET )
        Help( " " , 1 , "FA050CODRET" ,, STR0343, 1, 0 )  //"Codigo de retencao nao informado!"
        lRet	:= .F.
    endif

Return(lRet)

//-------------------------------------------------------------------------
/*/{Protheus.doc} FSubsMotBx
Funcao para criar automaticamente o motivo de baixa STP na tabela Mot baixas

@Param cMot, Nome do motivo de baixa a ser criado
@Param cNomMot, Descrição do motivo de baixa
@Param cConfMot, Caracteristicas do motivo de baixa
@Return cMot, Nome do motivo de baixa que sera usado na baixa dos titulos via ExecAuto

@author Vitor Duca
@since  19/12/2019
@version 12
/*/
//-------------------------------------------------------------------------
Static Function FSubsMotBx(cMot as Character, cNomMot as Character, cConfMot as Character)
	Local lMotBxEsp	As Logical
	Local aMotbx 	As Array
	Local nHdlMot	As Numeric
	Local nI		As Numeric
	Local cFile 	As Character
	Local nTamLn	As Numeric

    //Inicialização de variaveis
    lMotBxEsp	:= .F.
    aMotbx 	    := ReadMotBx(@lMotBxEsp)
    nHdlMot	    := 0
    nI			:= 0
    cFile 	    := "SIGAADV.MOT"
    nTamLn	    := 19

    if __lMotInDb == Nil
        __lMotInDb := AliasInDic("F7G")
    Endif

    If !__lMotInDb
	    If lMotBxEsp
	    	nTamLn	:= 20
	    	cConfMot	:= cConfMot + "N"
	    EndIf

	    If ExistBlock("FILEMOT")
	    	cFile := ExecBlock("FILEMOT",.F.,.F.,{cFile})
	    Endif

	    If Ascan(aMotbx, {|x| Substr(x,1,3) == Upper(cMot)}) < 1
	    	nHdlMot := FOPEN(cFile,FO_READWRITE)
	    	If nHdlMot <0
	    		HELP(" ",1,"SIGAADV.MOT")
	    		Final("SIGAADV.MOT")
	    	Endif

	    	nTamArq:=FSEEK(nHdlMot,0,2)
	    	FSEEK(nHdlMot,0,0)

	    	For nI:= 0 to  nTamArq step nTamLn
	    		xBuffer:=Space(nTamLn)
	    		FREAD(nHdlMot,@xBuffer,nTamLn)
	        Next

	    	fWrite(nHdlMot,cMot+cNomMot+cConfMot+chr(13)+chr(10))
	    	fClose(nHdlMot)
	    EndIf
    Endif

    FwFreeArray(aMotBx)
Return
//-------------------------------------------------------------------------
/*/{Protheus.doc} FSubsCpoU
Função para verificar os campos de usuario que serão considerados na substituição

@author Vitor Duca
@since  19/12/2019
@param aRestrict, Array, Matriz contendo os campos que serão criados na temporaria - FwTemporaryTable()
@param lStruTrb, Logic, Informa se a chamada está sendo feita para carregar os campos do usuário na tela de substituição
@version 12
/*/
//-------------------------------------------------------------------------
Static Function FSubsCpoU(aRestrict, lStruTrb)
    Local aArea     As Array
    Local aIniCpos  As Array
    Local aFields   As Array
    Local nPosCpo   As Numeric
    Local nX        As Numeric

    DEFAULT aRestrict   := {}        
    DEFAULT lStruTrb    := .F.
    DEFAULT __lF050SUB  := ExistBlock("FA050SUB")

    aArea       := GetArea()
    aIniCpos    := {}
    aFields     := {}
    nPosCpo     := 0
    nX          := 0

    aFields := FWSX3Util():GetAllFields("SE2")
    
    If !__lF050SUB .or. lStruTrb
        For nX := 1 to Len(aFields)
            If GetSx3Cache(aFields[nX],"X3_PROPRI") $ "UL" .and. GetSx3Cache(aFields[nX],"X3_CONTEXT") != "V" .and. GetSx3Cache(aFields[nX],"X3_TIPO") != "M"
                Aadd(__aIniCpos,aFields[nX])
                Aadd(aRestrict,aFields[nX])
            Endif        
        Next nX 
    Else
        aIniCpos := ExecBlock("FA050SUB",.f.,.f.)//array com nome de campos a serem inicializados
        For nX := 1 to Len(aIniCpos)
            nPosCpo := ASCAN(aFields,{|x|AllTrim(upper(x)) == AllTrim(upper(aIniCpos[nx])) })
            If nPosCpo > 0 .and. GetSx3Cache(aFields[nX],"X3_CONTEXT") != "V" .and. GetSx3Cache(aFields[nX],"X3_TIPO") != "M"
                Aadd(__aIniCpos,aIniCpos[nx])
            Endif
        Next
    Endif

    RestArea(aArea)
    FwFreeArray(aArea)
    FwFreeArray(aIniCpos)
    FwFreeArray(aFields)

Return

//-------------------------------------------------------------------------
/*/{Protheus.doc} FTemAbat
Função para verificar se o titulo selecionado na função F050Vlabt() ja possui
abatimento vinculado com o mesmo tipo que esta sendo incluido

@author Vitor Duca
@since  19/12/2019
@param cChavePai, Character, Chave do titulo pai (E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)
@version 12
/*/
//-------------------------------------------------------------------------
Static Function FTemAbat(cChavePai As Character, cTpAbat As Character) As Logical
    Local lRet      As Logical
    Local cQuery    As Character
    Local cFilOri   As Character

    Default cChavePai := ""
    Default cTpAbat   := ""

    lRet    := .F.
    cQuery  := ""
    cFilOri := ""

    If __oAbtQry == NIL
        cQuery := "SELECT E2_FILORIG FILORI "
        cQuery += "FROM "+RetSqlName("SE2")+" "
        cQuery += "WHERE E2_FILIAL = ? "
        cQuery += "AND E2_TITPAI = ? "
        cQuery += "AND E2_TIPO = ? "
        cQuery += "AND D_E_L_E_T_ = ' ' "
        cQuery := ChangeQuery(cQuery)
        __oAbtQry := FWPreparedStatement():New(cQuery)
    Endif
    
    __oAbtQry:SetString(1,xFilial("SE2"))
    __oAbtQry:SetString(2,cChavePai)
    __oAbtQry:SetString(3,cTpAbat)
    cQuery := __oAbtQry:GetFixQuery()

    cFilOri := MpSysExecScalar(cQuery,"FILORI")

    If !Empty(cFilOri)
        lRet := .T.
    Endif

Return lRet

//-------------------------------------------------------------------------
/*/{Protheus.doc} F050VRAT
Função para verificar se as duas opções de rateio foram selecionadas.

@author Matheus Ribeiro
@since  16/03/2020
@version 12
/*/
//-------------------------------------------------------------------------
Static Function F050VRAT()
    Local lRet := .T.

    IF M->E2_MULTNAT == "1" .AND. M->E2_RATEIO == "S"
        
        //Não é permitido fazer a utilização concomitante de 2 tipos de rateio. (Contábil e Multi-Naturezas)
        Help(" ",1,"F050VRAT",,STR0346,2,0,,,,,, {STR0347})
        
        lRet := .F.

    ENDIF

Return lRet

//-------------------------------------------------------------------------
/*/{Protheus.doc} f050LRatIR
Função para limpar o objeto __oRatIRF
Chamado pelo fina050 e FA050Natur

@author Karen
@since  08/04/2020
@version 12.1.30
@param lExclui, Logical, Define se ira excluir o Objeto da memoria
/*/
//-------------------------------------------------------------------------
Function f050LRatIR(lExclui as Logical)
    Default lExclui := .T.
    lRatOK	:= .T.

    If __oRatIRF <> Nil
        __oRatIRF:Clean()
        If lExclui
            FwFreeObj(__oRatIRF)
            __oRatIRF := Nil
        EndIf    
    EndIf

Return

//-------------------------------------------------------------------------
/*/{Protheus.doc} f050GRatIR
Função para retornar o objeto __oRatIRF
Chamado pelo fina050 e FA050Natur

@author Karen
@since  08/04/2020
@version 12.1.30
@return __oRatIrf, Object, Realiza o GET da variavel Static __oRatIRF para que outros fontes utilizem
/*/
//-------------------------------------------------------------------------
Function f050GRatIR()
Return __oRatIRF

//-------------------------------------------------------------------------
/*/{Protheus.doc} f050SRatIR
Função para setar o objeto __oRatIRF
Chamado pelo fina050 e FA050Natur

@author Karen
@since  08/04/2020
@version 12.1.30
@param oObj, Object, Objeto que sera atribuido ao oRatIrf
/*/
//-------------------------------------------------------------------------
Function f050SRatIR(obj as Object)
    __oRatIRF := obj
Return

//-------------------------------------------------------------------------
/*/{Protheus.doc} f050CRatIR
Função para criar o objeto __oRatIRF
Chamado pelo fina050 e FA050Natur

@author Karen
@since  08/04/2020
@version 12.1.30
@param lIRPFBaixa, Logical, Define se o fato gerador do imposto é caixa
/*/
//-------------------------------------------------------------------------
Function f050CRatIR(lIRPFBaixa as Logical)
    Local cCdRetIRRt    As Character

    Default lIRPFBaixa := .F.

    cCdRetIRRt  := ""
    __lRateioIR := .F.
    If __lLocBRA .and. FindFunction("FinXRatIR")
        
        cCdRetIRRt    := SuperGetMv("MV_RETIRRT",.T.,"3208")
        If __oRatIRF <> Nil
            f050LRatIR(.F.)
        EndIf
        If Alltrim(M->E2_CODRET) $ cCdRetIRRt
            If __oRatIRF == Nil
                __oRatIRF := FinBCRateioIR():New()
            EndIf
            __oRatIRF:SetFilOrig(cFilAnt)
            __oRatIRF:SetForLoja(SA2->A2_COD,SA2->A2_LOJA)
            __oRatIRF:SetIRBaixa(lIRPFBaixa)
            If Len(__oRatIRF:aRatIRF) > 1
                __lRateioIR := .T.
            EndIf
        EndIf    
    EndIf

Return __oRatIRF

//-------------------------------------------------------------------------
/*/{Protheus.doc} f050RatLeg
Função para verificar se houve retenção de IR progressivo no modelo anterior sem rateio por CPF
Essa função deverá ser retirada no próximo release 12.1.33
Chamado pelo fina050 

@param cFilSe2, caracter, Filial do título pai da SE2
@param cTitPai, caracter, TitPai para busca dos TXs

@return lRet, lógico, .T. não encontrado e .F. encontrou titulo legado

@author Karen Honda
@since  27/04/2020
@version 12.1.30
/*/
//-------------------------------------------------------------------------
Function f050RatLeg(cFilSe2 as Character, cTitPai as Character) as Logical
    Local lRet      as Logical
    Local cTipoIn   as Character
    Local cNatIrf   as Character
    Local cQuery    as Character
    Local cAliasTmp as Character

    Default cFilSe2 := cFilAnt
    Default cTitPai := ""


    lRet        := .T.
    cTipoIn     := FormatIn(MVTAXA + "|" +MVTXA  ,"|")
    cNatIrf     := &(SuperGetMv("MV_IRF",.F.,"'IRF'", cFilSe2))
    cAliasTmp   := ""
    cQuery := ""

    IF __oRatQry == NIL
        cQuery :=	"SELECT SE2.E2_CNPJRET, SE2.R_E_C_N_O_ RECNO"
        cQuery +=	" FROM ? SE2"
        cQuery += 	" WHERE SE2.E2_FILIAL = ?"
        cQuery += 	" AND SE2.E2_TITPAI = ?"
        cQuery += 	" AND SE2.E2_NATUREZ = ?"
        cQuery += 	" AND SE2.E2_TIPO IN " + cTipoIn
        cQuery +=	" AND SE2.D_E_L_E_T_ = ' '"
        cQuery := ChangeQuery(cQuery)
        __oRatQry := FWPreparedStatement():New(cQuery)
    EndIf

    __oRatQry:SetNumeric(1,RetSqlName("SE2"))
    __oRatQry:SetString(2,xFilial("SE2",cFilSe2))
    __oRatQry:SetString(3,cTitPai)
    __oRatQry:SetString(4,cNatIrf)
    cQuery := __oRatQry:GetFixQuery()	

    cAliasTmp := MpSysOpenQuery(cQuery)
        
    While (cAliasTmp)->(!Eof())
        If Len( Alltrim((cAliasTmp)->E2_CNPJRET) ) > 11
            lRet := .F.
            Exit
        EndIf
        (cAliasTmp)->(DbSkip())
    EndDo	
    (cAliasTmp)->(DbCloseArea())

Return lRet
//-------------------------------------------------------------------------
/*/{Protheus.doc} f050RatOk
Função para exibir mensagem no TudoOK se houver retenção de IR progressivo no modelo anterior sem rateio por CPF
Essa função deverá ser retirada no próximo release 12.1.33
Chamado pelo fina050 

@param lRet, lógico, .T. não encontrado e .F. encontrou titulo legado
@return lRet, lógico, .T. não encontrado e .F. encontrou titulo legado

@author Karen Honda
@since  27/04/2020
@version 12.1.30
/*/
//-------------------------------------------------------------------------
Function f050RatOk(lRet as Logical) as Logical
    Default lRet := .T.

    If !lRet 
        Help(,,"IRPROGLEGADO",,STR0350,1,0, NIL, NIL, NIL, NIL, NIL, {STR0351}) //"Existe(m) título(s) no mesmo período com cálculo de IR Progressivo sem o rateio por CPF, o que pode impactar no cálculo da cumulatividade na inclusão deste."
    EndIf

Return lRet

/*/{Protheus.doc} F050CodB
Tela para inclusão do código de barras (44 posições) ou linha digitável (47 ou 48 posições).

@author  Felipe Raposo
@version Protheus 12
@since   07/08/2019
/*/
Static Function F050CodB()

    Local oDialog  as object
    Local oGetCod  as object
    Local cCodDig  As Character
    Local cLeave   As Character
    Local cPicture As Character
    Local oSayCod  As Object
    Local oLayer   As Object
    Local bSetGet  As Codeblock
    Local bValid   As Codeblock
    Local oButton  As Object

    cCodDig  := Space(55)
    cLeave   := ""
    cPicture := "@R " + Replicate("9", 55)
    oLayer 	 := FWLayer():New()
    bSetGet  := {|u| If(PCount()>0,cCodDig:=u,cCodDig) }
    bValid   := {||(F050VldCB(cCodDig) .and. (oDialog:End(), .T.))}

    oDialog := MsDialog():New( 0, 0, 175, 600, "",,,, nOr( WS_VISIBLE, WS_POPUP ),,,,, .T.,,,, .F. )
	oLayer:Init( oDialog, .T. )
	oLayer:AddLine( "LINE01", 100 )
	oLayer:AddCollumn( "BOX01", 100,, "LINE01" )
	oLayer:AddWindow( "BOX01", "PANEL01", STR0007 + ' - ' + STR0332, 100, .F.,,, "LINE01" )
    oSayCod	:= TSay():New(03,10,{|| STR0332 },oLayer:GetWinPanel ( 'BOX01' , 'PANEL01', 'LINE01' ),,,,,,.T.,,,87,07,,,,,,.T.)
    oGetCod := TGet():New( 01, 105,bSetGet,oLayer:GetWinPanel ( 'BOX01' , 'PANEL01', 'LINE01' ), 182, 10,cPicture,/*bValid*/,,,,,,.T.,,,,,,,,,,cCodDig,,,,,,,,,,,,,)
    oButton := TButton():New(20, 250,"OK",oLayer:GetWinPanel ( 'BOX01' , 'PANEL01', 'LINE01' ),bValid,30,10,,,,.T.)
    oDialog:Activate(,,,.T.)

Return

/*/{Protheus.doc} F050VldCB
Valida a digitação do código de barras (44 posições) ou linha digitável (47 ou 48 posições).

@author  Felipe Raposo
@version Protheus 12
@since   07/08/2019
/*/
Static Function F050VldCB(cCodDig As Character) As Logical

    Local lRet as logical

    // Valida o código digitado.
    // Se o código for válido, atualiza campos do título.
    lRet := VldCodBar(@cCodDig, .T., .F.)
    If lRet
        // Se for código de barras, tem 44 posições.
        // Se tiver 47 ou 48 posições, é uma linha digitável.
        If (len(cCodDig) = 44)
            M->E2_CODBAR := PadR(cCodDig, 44)
            M->E2_LINDIG := PadR(FinCBLD(cCodDig), 48)
        Else
            M->E2_CODBAR := PadR(FinLDCB(cCodDig), 44)
            M->E2_LINDIG := PadR(cCodDig, 48)
        Endif
    Else
        M->E2_CODBAR := CriaVar("E2_CODBAR", .F.)
        M->E2_LINDIG := CriaVar("E2_LINDIG", .F.)
    Endif

Return lRet

/*/{Protheus.doc} Clean050Mr
    Função responsavel por efetuar a limpeza da variaveis
    estaticas do configurador de tributos

    @author Vitor Duca
    @version Protheus 12
    @since   19/01/2021
/*/
Function Clean050Mr()
    __lPccMR := .F.
    __lIrfMR := .F.
    __lInsMR := .F.
    __lIssMR := .F.
    __lCidMR := .F.
    __lSestMR := .F.
    __lOtImpMR := .F.
    __aVetImp := {}
    __nVlrMR := 0
    __lPccBxMR := .F.
    __lIrfBxMR := .F.
    __lGrvMR := .F.
return

//-------------------------------------------------------------------
/*/{Protheus.doc}GetPerPCC
Verificar o percentual de cada imposto do PCC

@author Rene Julian
@since 05/03/2021
@type function

@param cNatur, characters, Código da natureza financeira

@return aRet - Vetor com os percentuais do PIS, COFINS e CSLL
@sample aRet[1] = Percentual do PIS
		aRet[2] = Percentual do COFINS
		aRet[3] = Percentual do CSLL
/*/
//-------------------------------------------------------------------
Static Function GetPerPCC(cNatur as Character ) as Array 
Local aPercent As Array 
Local aAreaSED As Array 

Default cNatur := ""

aPercent := Array(3) 		
aAreaSED := SED->( GetArea() )

aFill(aPercent,0)

SED->( dbSetOrder(1) ) //ED_FILIAL+ED_CODIGO
If !Empty(cNatur) .And. SED->( msSeek( xFilial("SED") + cNatur ) )
    If SED->ED_CALCPIS == "S"  
        aPercent[1]	:= SED->ED_PERCPIS / 100
    EndIf
    If SED->ED_CALCCOF == "S"
        aPercent[2]	:= SED->ED_PERCCOF / 100
    EndIf
    If SED->ED_CALCCSL == "S"
        aPercent[3]	:= SED->ED_PERCCSL / 100
    EndIf
EndIf

Restarea(aAreaSED)
FwFreeArray(aAreaSED)	

Return aPercent

//-------------------------------------------------------------------
/*/{Protheus.doc}F050RatAut
Validação tudo ok rateio

@author rafaelrondon
@since 05/05/2021
@type function

@param cLote, characters, Código lote

@return lRet - Rateio está válido?

/*/
//-------------------------------------------------------------------
Function F050RatAut(cLote As Character) As Logical

Local lRet As Logical

lRet := .T.

If M->E2_RATEIO == "S"
    If !ExistBlock("F050RAUT")
        lRet := fa050TudCT(3,"511","FINA050")
    EndIf
    If lRet
        F050EscRat("511","FINA050",cLote)
    EndIf
EndIf

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} FChkTCExec
Verifica status da atualização de tabelas temporarias

@author Totvs
@since  26/07/2022
@version 12
/*/
//-------------------------------------------------------------------
Static Function FChkTCExec(nStatus As Numeric, nIdObject As Numeric)

//--------------------------------------------
// Objeto __oFIN0501
If nIdObject == 1 
    If nStatus < 0 .Or. Select("TMP")<=0
        __oFIN0501:Delete()
        __oFIN0501 := NIL
        __cFIN1Name:= "" 
    Else
        TMP->(DBGOTO(1))
    Endif
EndIf

//--------------------------------------------
// Objeto __oFIN0502
If nIdObject == 2 
    If nStatus < 0 .Or. Select(cAliasSE2)<=0
        __oFIN0502:Delete()
        __oFIN0502 := NIL
        __cFIN2Name:= ""
    Else
        (cAliasSE2)->(DBGOTO(1))
	Endif
EndIF

Return

//-------------------------------------------------------------------
/*/{Protheus.doc} FGetHashR
Funcao de Get da variavel Statica oHashREINF

@author Leonardo Castro
@since 30/08/2019
@version 12.1.25
@return STATIC __oHsREINF
/*/
//-------------------------------------------------------------------
Function FGetHashR()
Return __oHsREINF


//-------------------------------------------------------------------------
/*/{Protheus.doc} FGaRatIrA
Função para obter o rateio IRRF Aluguel
Chamado pela FINM070

@author Pequim
@since  26/12/2022
@version 12.1.2210

/*/
//-------------------------------------------------------------------------
Function FGaRatIrA(aRatIrf)
    DEFAULT aRatIrf := {}
    If __lRateioIR
        aRatIrf := __oRatIRF:aRatIRF
    Endif
Return

//-------------------------------------------------------------------------
/*/{Protheus.doc} FinLoadSX3
    Funcao que utiliza API e Data Cached de dicionário para acesso de informações SX3
    
@version 1.0
@param cTable, characters, Para localização do X3_ARQUIVO
@param bFor, Block, Bloco de código com critérios para a construção do retorno
@param aSX3Cols, Array, Lista de Campos SX3 para retorno
@return ${return}, aRet, Array, Pode ser uma lista simples de campos do cTable ou 
                                uma lista composta dos cammpos cTable e o conteúdo das suas propriedades SX3 
@type function
//-------------------------------------------------------------------------
/*/
Static Function FinLoadSX3( cTable As Character,;
                            bFor As Block,;
                            aSX3Cols As Array) As Array
    Local nI As Numeric     //-- Controle de Interações
    Local nJ As Numeric     //-- Controle de Interações
    Local aFields As Array  //-- Campos do dicionário SX3 relativos a cTable
    Local aValue As Array   //-- Propriedades SX3/Usuário
    Local aRet As Array     //-- Estrutura de Dados de Retorno
    Local aColsRet as Array

    Default bFor := {|| .T.}    //-- Critério de seleção

    If !Empty(cTable) .And. Empty(__cLocTit)
        //-- Lozalização para retorno dos Títulos
        If "es" $ FWRetIdiom()
            __cLocTit := "X3_TITSPA"
        Else
            If "en" $ FWRetIdiom()
                __cLocTit := "X3_TITENG"
            Else
                __cLocTit := "X3_TITULO"
            EndIf
        EndIf
    EndIf

    If !Empty(cTable)
        aFields := FWSX3Util():GetAllFields(cTable)
        aRet := {}
        aValue := Array(1)  //-- Sempre e somente 1 elemento 

        For nI := 1 To Len(aFields)
            If Eval(bFor,aFields[nI])
    
                aColsRet := {}
                For nJ := 1 To Len(aSX3Cols)
                    If "X3_TIT" $ aSX3Cols[nJ][1]
                        aValue[1] := GetSX3Cache(aFields[nI],__cLocTit)
                    ElseIf "X3_" $ aSX3Cols[nJ][1]
                        aValue[1] := GetSX3Cache(aFields[nI],aSX3Cols[nJ][1])
                    Else    //-- Conteúdos Específicos
                        aValue[1] := aSX3Cols[nJ][1]
                    EndIf
                    If ValType(aSX3Cols[nJ][2]) == 'B'
                        aValue[1] := Eval(aSX3Cols[nJ][2],aValue[1])
                    EndIf
                    AAdd(aColsRet,aValue[1])
                Next nJ
                AADD(aRet, If(Len(aColsRet) > 1, aColsRet, aColsRet[1]))

            EndIf
        Next nI
    EndIf

Return aRet

/*/{Protheus.doc} F050RatDsd
    Realiza tratamento do rateio contábil no desdobramento quando o titulo possui impostos
    @type  Static Function
    @author Vitor Duca
    @since 22/06/2023
    @version 1.0
    @return aRatCC, Array, Estrutura do array que sera enviado para a execauto para rateio contabil
/*/
Static Function F050RatDsd() As Array
    Local nY As Numeric
    Local aRatAux As Array
    Local aRatCC As Array

    nY := 0
    aRatAux := {}
    aRatCC := {}

    If Select("TMP") > 0 
        TMP->(DbGoTop())
        While TMP->(!Eof())
            aRatAux := {}
            For nY := 1 To TMP->(FCount())
                If TMP->(FieldName(nY)) <> "CTJ_VALOR" .and. !Empty(TMP->(FieldGet(nY))) 
                    aAdd(aRatAux, {TMP->(FieldName(nY)), TMP->(FieldGet(nY)), Nil})
                Endif   
            Next
            aAdd(aRatCC, aRatAux)
            TMP->(DbSkip())
        End Do
    Endif
Return aRatCC

//-------------------------------------------------------------------------
/*/{Protheus.doc} f050IRSimp
Função para retornar se o calculo do IR de pessoa fisica foi calculado
através da dedução simplificada (MP 1.171/23). Chamado pelo FINA986

@author fabio.casagrande
@since  28/06/2023
@return __lDedSimpl, Logical, Realiza o GET da variavel Static __lDedSimpl 
para que outros fontes utilizem
/*/
//-------------------------------------------------------------------------
Function f050IRSimp()
    If __lDedSimpl == Nil
        __lDedSimpl := .F.
    EndIf
Return __lDedSimpl
