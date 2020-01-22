      SUBROUTINE CLASSW(THLIQ,  THICE,  TBAR,   TCAN,   RCAN,   SNCAN,
     1                  RUNOFF, TRUNOF, SNO,    TSNOW,  RHOSNO, ALBSNO, 
     2                  WSNOW,  ZPOND,  TPOND,  GROWTH, TBASE,  GFLUX,
     3                  PCFC,   PCLC,   PCPN,   PCPG,   QFCF,   QFCL,
     4                  QFN,    QFG,    QFC,    HMFC,   HMFG,   HMFN,
     5                  HTCC,   HTCS,   HTC,    ROFC,   ROFN,   ROVG, 
     6                  WTRS,   WTRG,   OVRFLW, SUBFLW, BASFLW, 
     7                  TOVRFL, TSUBFL, TBASFL, EVAP,   
     8                  TBARC,  TBARG,  TBARCS, TBARGS, THLIQC, THLIQG, 
     9                  THICEC, THICEG, HCPC,   HCPG,   RPCP,   TRPCP,  
     A                  SPCP,   TSPCP,  PCPR,   TA,     RHOSNI, GGEO,
     B                  FC,     FG,     FCS,    FGS,    TPONDC, TPONDG,
     C                  TPNDCS, TPNDGS, EVAPC,  EVAPCG, EVAPG,  EVAPCS,
     D                  EVPCSG, EVAPGS, QFREZC, QFREZG, QMELTC, QMELTG,
     E                  RAICAN, SNOCAN, RAICNS, SNOCNS, FROOT,  FSVF,   
     F                  FSVFS,  CWLCAP, CWFCAP, CWLCPS, CWFCPS, TCANO,  
     G                  TCANS,  CHCAP,  CHCAPS, CMASSC, CMASCS, ZSNOW,  
     H                  GZEROC, GZEROG, GZROCS, GZROGS, G12C,   G12G,
     I                  G12CS,  G12GS,  G23C,   G23G,   G23CS,  G23GS,
     J                  TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS,
     K                  ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS, TSFSAV,
     L                  TCTOPC, TCBOTC, TCTOPG, TCBOTG, 
     M                  THPOR,  THLRET, THLMIN, BI,     PSISAT, GRKSAT,
     N                  THLRAT, THFC,   XDRAIN, HCPS,   DELZ,   
     O                  DELZW,  ZBOTW,  XSLOPE, XDRAINH, WFSURF, KSAT,
     P                  ISAND,  IGDR,
     Q                  IWF,    ILG,    IL1,    IL2,    N,
     R                  JL,     IC,     IG,     IGP1,   IGP2,
     S                  NLANDCS,NLANDGS,NLANDC, NLANDG, NLANDI, 
     S                  MANNING_N, DD,NCOUNT,t0_ACC,
     4                  SI,TSI,INFILTYPE,SNOWMELTD,SNOWMELTD_LAST,
     5                  MELTRUNOFF,SNOWINFIL,
     6                  CUMSNOWINFILCS,CUMSNOWINFILGS,
     7                  SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS, FRZC,
     +                  CMIN,      CMAX,      B,         K1,        K2,
     1                  ZPNDPRECS, ZPONDPREC, ZPONDPREG, ZPNDPREGS, 
     2                  UM1CS,     UM1C,      UM1G,      UM1GS, 
     3                  QM1CS,     QM1C,      QM1G,      QM1GS, 
     4                  QM2CS,     QM2C,      QM2G,      QM2GS,  UMQ,
     5                  FSTRCS,    FSTRC,     FSTRG,     FSTRGS,
     +                  PBSMFLAG,
     1                  ZSNOCS,ZSNOGS,ZSNOWC,ZSNOWG,
     2                  HCPSCS,HCPSGS,HCPSC,HCPSG,
     3                  TSNOWC,TSNOWG,RHOSC,RHOSG,
     4                  XSNOWC,XSNOWG,XSNOCS,XSNOGS)
C                                                                        
C     * DEC 09/11 - M.MEKONNEN. FOR PDMROF.
C     * OCT 18/11 - M.LAZARE.   PASS IN IGDR THROUGH CALLS TO
C     *                         GRDRAN/GRINFL (ORIGINATES NOW
C     *                         IN CLASSB - ONE CONSISTENT
C     *                         CALCULATION).                                                                          
C     * APR 04/11 - D.VERSEGHY. ADD DELZ TO GRINFL CALL.
C     * OCT 01/10 - M.MACDONALD.ONLY PERFORM VANISHINGLY SMALL SNOW
C                               CALCULATIONS IF PBSM NOT BEING USED (MESH OPTION)
C     * DEC 07/09 - D.VERSEGHY. ADD RADD AND SADD TO WPREP CALL.
C     * JAN 06/09 - D.VERSEGHY. INCREASE LIMITING SNOW AMOUNT.
C     * JUN    08 - C.THOMPSON. ADDED CALLS TO WATROF.
C     * FEB 25/08 - D.VERSEGHY. MODIFICATIONS REFLECTING CHANGES
C     *                         ELSEWHERE IN CODE.
C     * MAR 23/06 - D.VERSEGHY. CHANGES TO ADD MODELLING OF WSNOW;
C     *                         PASS IN GEOTHERMAL HEAT FLUX.
C     * MAR 21/06 - P.BARTLETT. PASS ADDITIONAL VARIABLES TO WPREP.
C     * DEC 07/05 - D.VERSEGHY. REVISIONS TO CALCULATION OF TBASE.
C     * OCT 05/05 - D.VERSEGHY. MODIFICATIONS TO ALLOW OPTION OF SUB-
C     *                         DIVIDING THIRD SOIL LAYER.
C     * MAR 23/05 - D.VERSEGHY. ADD VARIABLES TO SUBROUTINE CALLS.
C     * MAR 14/05 - D.VERSEGHY. RENAME SCAN TO SNCAN (RESERVED NAME
C     *                         IN F90).
C     * NOV 04/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUL 08/04 - D.VERSEGHY. NEW LOWER LIMITS FOR RCAN, SCAN, ZPOND
C     *                         AND SNOW.
C     * DEC 09/02 - D.VERSEGHY. SWITCH CALLING ORDER OF TFREEZ AND
C     *                         SNOVAP FOR CONSISTENCY WITH DIAGNOSTICS.
C     * SEP 26.02 - D.VERSEGHY. CHANGED CALL TO SUBCAN.
C     * AUG 01/02 - D.VERSEGHY. ADD CALL TO WATROF, NEW SUBROUTINE
C     *                         CONTAINING WATERLOO OVERLAND FLOW
C     *                         AND INTERFLOW CALCULATIONS.
C     *                         SHORTENED CLASS3 COMMON BLOCK.
C     * JUL 03/02 - D.VERSEGHY. STREAMLINE SUBROUTINE CALLS; MOVE 
C     *                         CALCULATION OF BACKGROUND SOIL 
C     *                         PROPERTIES INTO "CLASSB"; CHANGE
C     *                         RHOSNI FROM CONSTANT TO VARIABLE.
C     * OCT 04/01 - M.LAZARE.   REMOVE SEVERAL OLD DIAGNOSTIC FIELDS
C     *                         AND ADD NEW FIELD "ROVG".
C     * MAY 14/01 - M.LAZARE.   ADD CALLS TO SUBROUTINE "SNOVAP" FOR
C     *                         FC AND FG SUBAREAS OF GRID CELL.
C     * OCT 20/00 - D.VERSEGHY. ADD WORK ARRAY "RHOMAX" FOR SNOALBW.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         CHANGES RELATED TO VARIABLE SOIL DEPTH
C     *                         (MOISTURE HOLDING CAPACITY) AND DEPTH-
C     *                         VARYING SOIL PROPERTIES.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS; INTRODUCE CALCULATION OF
C     *                         OVERLAND FLOW.
C     * AUG 30/95 - D.VERSEGHY. CLASS - VERSION 2.4. 
C     *                         VARIABLE SURFACE DETENTION CAPACITY
C     *                         IMPLEMENTED.
C     * AUG 24/95 - D.VERSEGHY. UPDATE ARRAY "EVAP" TO TAKE INTO 
C     *                         ACCOUNT "WLOST"; RATIONALIZE 
C     *                         CALCULATION OF THE LATTER.
C     *                         COMPLETION OF WATER BUDGET DIAGNOSTICS.
C     * AUG 18/95 - D.VERSEGHY. REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL 
C     *                         ORGANIC MATTER CONTENT.
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         CHANGES TO SUBROUTINE CALLS ASSOCIATED
C     *                         WITH REVISIONS TO DIAGNOSTICS.
C     *                         ALLOW SPECIFICATION OF LIMITING POND
C     *                         DEPTH "PNDLIM" (PARALLEL CHANGES MADE
C     *                         SIMULTANEOUSLY IN TMCALC).
C     * DEC 16/94 - D.VERSEGHY. TWO NEW DIAGNOSTIC FIELDS.
C     * NOV 18/93 - D.VERSEGHY. LOCAL VERSION WITH INTERNAL WORK ARRAYS
C     *                         HARD-CODED FOR USE ON PCS.
C     * NOV 01/93 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         REVISIONS ASSOCIATED WITH NEW VERSION
C     *                         OF TMCALC.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. NUMEROUS NEW DIAGNOSTIC FIELDS.
C     * MAY 06/93 - D.VERSEGHY/M.LAZARE. CORRECT BUG IN CALL TO TMCALC
C     *                                  FOR CANOPY-SNOW CASE, WHERE
C     *                                  SHOULD BE PASSING "HCPCS"
C     *                                  INSTEAD OF "HCPGS". 
C     * MAY 15/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C                               CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. LAND SURFACE WATER BUDGET CALCULATIONS.
C                                                                                 
      USE FLAGS
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,IC,IG,IGP1,IGP2,I,J,NLANDCS,NLANDGS,
     1        NLANDC,NLANDG,NLANDI,IPTBAD,JPTBAD,KPTBAD,LPTBAD,N
      INTEGER IWF(ILG)
C
C     * MAIN OUTPUT FIELDS.
C                                                                                  
      REAL THLIQ (ILG,IG), THICE (ILG,IG), TBAR  (ILG,IG), 
     1     GFLUX (ILG,IG)
C
      REAL TCAN  (ILG),    RCAN  (ILG),    SNCAN (ILG),    RUNOFF(ILG),
     1     SNO   (ILG),    TSNOW (ILG),    RHOSNO(ILG),    ALBSNO(ILG),
     2     ZPOND (ILG),    TPOND (ILG),    GROWTH(ILG),    TBASE (ILG),
     3     TRUNOF(ILG),    WSNOW (ILG)
C
C     * DIAGNOSTIC ARRAYS.
C
      REAL PCFC  (ILG),    PCLC  (ILG),    PCPN  (ILG),    PCPG  (ILG),    
     1     QFCF  (ILG),    QFCL  (ILG),    QFN   (ILG),    QFG   (ILG),    
     2     HMFC  (ILG),    HMFN  (ILG),    HTCC  (ILG),    HTCS  (ILG),    
     3     ROFC  (ILG),    ROFN  (ILG),    ROVG  (ILG),    WTRS  (ILG),    
     4     WTRG  (ILG),    OVRFLW(ILG),    SUBFLW(ILG),    BASFLW(ILG),
     5     TOVRFL(ILG),    TSUBFL(ILG),    TBASFL(ILG),    EVAP  (ILG)
C
      REAL QFC   (ILG,IG), HMFG  (ILG,IG), HTC   (ILG,IG)
C
C     * I/O FIELDS PASSED THROUGH CLASS.
C
      REAL RPCP  (ILG),   TRPCP (ILG),   SPCP  (ILG),   TSPCP (ILG),
     1     PCPR  (ILG),   TA    (ILG)
C
      REAL TBARC(ILG,IG), TBARG(ILG,IG), TBARCS(ILG,IG),TBARGS(ILG,IG),                      
     1     THLIQC(ILG,IG),THLIQG(ILG,IG),THICEC(ILG,IG),THICEG(ILG,IG),           
     2     HCPC  (ILG,IG),HCPG  (ILG,IG),TCTOPC(ILG,IG),TCBOTC(ILG,IG),
     3     TCTOPG(ILG,IG),TCBOTG(ILG,IG),FROOT (ILG,IG),TSFSAV(ILG,4)
C
      REAL FC    (ILG),   FG    (ILG),   FCS   (ILG),   FGS   (ILG),
     1     TPONDC(ILG),   TPONDG(ILG),   TPNDCS(ILG),   TPNDGS(ILG),              
     2     EVAPC (ILG),   EVAPCG(ILG),   EVAPG (ILG),   EVAPCS(ILG),               
     3     EVPCSG(ILG),   EVAPGS(ILG),   QFREZC(ILG),   QFREZG(ILG),           
     4     QMELTC(ILG),   QMELTG(ILG),   RAICAN(ILG),   SNOCAN(ILG),   
     5     RAICNS(ILG),   SNOCNS(ILG),   FSVF  (ILG),   FSVFS (ILG),   
     6     CWLCAP(ILG),   CWFCAP(ILG),   CWLCPS(ILG),   CWFCPS(ILG),   
     7     TCANO (ILG),   TCANS (ILG),   CHCAP (ILG),   CHCAPS(ILG),   
     8     CMASSC(ILG),   CMASCS(ILG),   ZSNOW (ILG),   RHOSNI(ILG),            
     9     GZEROC(ILG),   GZEROG(ILG),   GZROCS(ILG),   GZROGS(ILG),              
     A     G12C  (ILG),   G12G  (ILG),   G12CS (ILG),   G12GS (ILG),               
     B     G23C  (ILG),   G23G  (ILG),   G23CS (ILG),   G23GS (ILG),
     C     TSNOCS(ILG),   TSNOGS(ILG),   WSNOCS(ILG),   WSNOGS(ILG),
     D     RHOSCS(ILG),   RHOSGS(ILG),   ZPLIMC(ILG),   ZPLIMG(ILG),
     E     ZPLMCS(ILG),   ZPLMGS(ILG),   GGEO  (ILG)
C
C     * SOIL PROPERTY ARRAYS.
C
      REAL THPOR (ILG,IG),THLRET(ILG,IG),THLMIN(ILG,IG),BI    (ILG,IG),
     1     GRKSAT(ILG,IG),PSISAT(ILG,IG),THLRAT(ILG,IG),
     2     THFC  (ILG,IG),HCPS  (ILG,IG),DELZW (ILG,IG),DELZZ (ILG,IG),
     3     ZBOTW (ILG,IG),XDRAIN(ILG),   XSLOPE(ILG),   XDRAINH(ILG),   
     4     WFSURF(ILG),   KSAT(ILG),   DELZ  (IG), BULK_FC(ILG,IG)
C
      INTEGER             ISAND(ILG,IG), IGDR  (ILG)
C
C     * INTERNAL WORK ARRAYS USED THROUGHOUT CLASSW.
C
      REAL TBARWC(ILG,IG),TBARWG(ILG,IG),TBRWCS(ILG,IG),TBRWGS(ILG,IG),
     1     THLQCO(ILG,IG),THLQGO(ILG,IG),THLQCS(ILG,IG),THLQGS(ILG,IG),        
     2     THICCO(ILG,IG),THICGO(ILG,IG),THICCS(ILG,IG),THICGS(ILG,IG),        
     3     HCPCO (ILG,IG),HCPGO (ILG,IG),HCPCS (ILG,IG),HCPGS (ILG,IG),
     4     GRKSC (ILG,IG),GRKSG (ILG,IG),GRKSCS(ILG,IG),GRKSGS(ILG,IG),
     5     GFLXC (ILG,IG),GFLXG (ILG,IG),GFLXCS(ILG,IG),GFLXGS(ILG,IG)
C
      REAL SPCC  (ILG),   SPCG  (ILG),   SPCCS (ILG),   SPCGS (ILG),
     1     TSPCC (ILG),   TSPCG (ILG),   TSPCCS(ILG),   TSPCGS(ILG),
     2     RPCC  (ILG),   RPCG  (ILG),   RPCCS (ILG),   RPCGS (ILG),
     3     TRPCC (ILG),   TRPCG (ILG),   TRPCCS(ILG),   TRPCGS(ILG), 
     4     EVPIC (ILG),   EVPIG (ILG),   EVPICS(ILG),   EVPIGS(ILG),
     5     ZPONDC(ILG),   ZPONDG(ILG),   ZPNDCS(ILG),   ZPNDGS(ILG),
     6     XSNOWC(ILG),   XSNOWG(ILG),   XSNOCS(ILG),   XSNOGS(ILG),
     7     ZSNOWC(ILG),   ZSNOWG(ILG),   ZSNOCS(ILG),   ZSNOGS(ILG),
     8     ALBSC (ILG),   ALBSG (ILG),   ALBSCS(ILG),   ALBSGS(ILG),
     9     RHOSC (ILG),   RHOSG (ILG),   
     A     HCPSC (ILG),   HCPSG (ILG),   HCPSCS(ILG),   HCPSGS(ILG),
     B     RUNFC (ILG),   RUNFG (ILG),   RUNFCS(ILG),   RUNFGS(ILG),
     C     TRUNFC(ILG),   TRUNFG(ILG),   TRNFCS(ILG),   TRNFGS(ILG),
     D     TBASC (ILG),   TBASG (ILG),   TBASCS(ILG),   TBASGS(ILG)
C
      REAL SUBLC (ILG),   SUBLCS(ILG),   WLOSTC(ILG),   WLOSTG(ILG),
     1     WLSTCS(ILG),   WLSTGS(ILG),   RAC   (ILG),   RACS  (ILG),
     2     SNC   (ILG),   SNCS  (ILG),   TSNOWC(ILG),   TSNOWG(ILG), 
     3     DT    (ILG),   ZERO  (ILG),   RALB  (ILG),   ZFAV  (ILG),
     4     THLINV(ILG)
C
      INTEGER             LZFAV (ILG)
C
C     * INTERNAL WORK ARRAYS FOR WPREP AND CANADD.
C
      REAL RADD  (ILG),    SADD  (ILG)
C
C     * INTERNAL WORK FIELDS FOR GRINFL/GRDRAN (AND THEIR CALLED
C     * ROUTINES (I.E. WFILL,WFLOW,WEND) AND ICEBAL.
C
      REAL ZMAT  (ILG,IGP2,IGP1)
C
      REAL WMOVE (ILG,IGP2),   TMOVE (ILG,IGP2)
C
      REAL THLIQX(ILG,IGP1),   THICEX(ILG,IGP1),   TBARWX(ILG,IGP1),
     1     DELZX (ILG,IGP1),   ZBOTX (ILG,IGP1),   FDT   (ILG,IGP1),
     2     TFDT  (ILG,IGP1),   PSIF  (ILG,IGP1),   THLINF(ILG,IGP1),   
     3     GRKINF(ILG,IGP1),   FDUMMY(ILG,IGP1),   TDUMMY(ILG,IGP1),
     4     ZRMDR (ILG,IGP1)
C
      REAL THLMAX(ILG,IG),     THTEST(ILG,IG),     THLDUM(ILG,IG),
     1     THIDUM(ILG,IG),     TDUMW (ILG,IG)
C
      REAL TRMDR (ILG),    ZF    (ILG),    FMAX  (ILG),    TUSED (ILG),
     1     RDUMMY(ILG),    WEXCES(ILG),    FDTBND(ILG),    WADD  (ILG),
     2     TADD  (ILG),    WADJ  (ILG),    TIMPND(ILG),    DZF   (ILG),
     3     DTFLOW(ILG),    THLNLZ(ILG),    THLQLZ(ILG),    DZDISP(ILG),
     4     WDISP (ILG),    WABS  (ILG),    ZMOVE (ILG),    TBOT  (ILG)
C
      INTEGER              IGRN  (ILG),    IGRD  (ILG),    IZERO (ILG),    
     1                     IFILL (ILG),    LZF   (ILG),    NINF  (ILG),    
     2                     IFIND (ILG),    ITER  (ILG),    NEND  (ILG),    
     3                     ISIMP (ILG),    ICONT (ILG)
C
C     * INTERNAL WORK ARRAYS FOR CANVAP AND SNOALBW.
C
      REAL EVLOST(ILG),    RLOST (ILG),    RHOMAX(ILG)
C
      INTEGER              IROOT (ILG)
C
C     * INTERNAL WORK ARRAYS FOR WATROF.
C
      REAL THCRIT(ILG,IG), DODRN (ILG),     DOVER (ILG),
     1     DIDRN (ILG,IG), DIDRNMX(ILG,IG),  DD(ILG),
     2     MANNING_N(ILG)
C
C     * INTERNAL WORK ARRAYS FOR CHKWAT.
C
      REAL BAL   (ILG)
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,RHOSOL,RHOOM,
     1     HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,SPHICE,SPHVEG,
     2     SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP
C
      COMMON /CLASS1/ DELT,TFREZ                                               
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
      
      INTEGER NCOUNT,INFILTYPE(ILG)
      REAL    SI(ILG),TSI(ILG),SNOWMELTD(ILG),
     3        SNOWMELTD_LAST(ILG),SNOWINFIL(ILG),CUMSNOWINFILCS(ILG),
     4        MELTRUNOFF(ILG),FRZC(ILG),
     5        CUMSNOWINFILGS(ILG)
      REAL    t0_ACC, SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS
      
C     * PDMROF
      REAL CMIN(ILG), CMAX(ILG), B(ILG), K1(ILG), K2(ILG)

      REAL ZPNDPRECS(ILG),ZPONDPREC(ILG),ZPONDPREG(ILG),ZPNDPREGS(ILG),
     1     UM1CS    (ILG),UM1C     (ILG),UM1G     (ILG),UM1GS    (ILG), 
     2     QM1CS    (ILG),QM1C     (ILG),QM1G     (ILG),QM1GS    (ILG), 
     3     QM2CS    (ILG),QM2C     (ILG),QM2G     (ILG),QM2GS    (ILG),
     4     UMQCS    (ILG),UMQC     (ILG),UMQG     (ILG),UMQGS    (ILG),
     5     UMQ      (ILG),
     6     FSTRCS   (ILG),    FSTRC(ILG),    FSTRG(ILG),   FSTRGS(ILG),
     *     CSTRCS(ILG), CSTRC(ILG), CSTRG(ILG), CSTRGS(ILG)
C
C     * PBSM
C
      logical, intent(in) :: PBSMFLAG

      UMQ   = 0.0
      UMQCS = 0.0
      UMQC  = 0.0
      UMQG  = 0.0
      UMQGS = 0.0
C
C-----------------------------------------------------------------------
C     * PREPARATION.
C
      CALL WPREP(THLQCO, THLQGO, THLQCS, THLQGS, THICCO, THICGO,
     1           THICCS, THICGS, HCPCO,  HCPGO,  HCPCS,  HCPGS,
     2           GRKSC,  GRKSG,  GRKSCS, GRKSGS,
     3           SPCC,   SPCG,   SPCCS,  SPCGS,  TSPCC,  TSPCG,
     4           TSPCCS, TSPCGS, RPCC,   RPCG,   RPCCS,  RPCGS,
     5           TRPCC,  TRPCG,  TRPCCS, TRPCGS, EVPIC,  EVPIG,
     6           EVPICS, EVPIGS, ZPONDC, ZPONDG, ZPNDCS, ZPNDGS,
     7           XSNOWC, XSNOWG, XSNOCS, XSNOGS, ZSNOWC, ZSNOWG,
     8           ZSNOCS, ZSNOGS, ALBSC,  ALBSG,  ALBSCS, ALBSGS,
     9           RHOSC,  RHOSG,  HCPSC,  HCPSG,  HCPSCS, HCPSGS, 
     A           RUNFC,  RUNFG,  RUNFCS, RUNFGS,
     B           TRUNFC, TRUNFG, TRNFCS, TRNFGS, TBASC,  TBASG,  
     C           TBASCS, TBASGS, GFLXC,  GFLXG,  GFLXCS, GFLXGS,
     D           SUBLC,  SUBLCS, WLOSTC, WLOSTG, WLSTCS, WLSTGS,
     E           RAC,    RACS,   SNC,    SNCS,   TSNOWC, TSNOWG,
     F           OVRFLW, SUBFLW, BASFLW, TOVRFL, TSUBFL, TBASFL, 
     G           PCFC,   PCLC,   PCPN,   PCPG,   QFCF,   QFCL,
     H           QFN,    QFG,    QFC,    HMFG,   
     I           ROVG,   ROFC,   ROFN,   TRUNOF, 
     J           THLIQX, THICEX, THLDUM, THIDUM,
     K           DT,     RDUMMY, ZERO,   IZERO,  DELZZ,
     L           FC,     FG,     FCS,    FGS,    
     M           THLIQC, THLIQG, THICEC, THICEG, HCPC,   HCPG,
     N           TBARC,  TBARG,  TBARCS, TBARGS, TBASE,  TSFSAV,
     O           FSVF,   FSVFS,  RAICAN, SNOCAN, RAICNS, SNOCNS, 
     P           EVAPC,  EVAPCG, EVAPG,  EVAPCS, EVPCSG, EVAPGS, 
     Q           RPCP,   TRPCP,  SPCP,   TSPCP,  RHOSNI, ZPOND,  
     R           ZSNOW,  ALBSNO, WSNOCS, WSNOGS, RHOSCS, RHOSGS,
     S           THPOR,  HCPS,   GRKSAT, ISAND,  DELZW,  DELZ,
     T           ILG,    IL1,    IL2,    JL,     IG,     IGP1,
     U           NLANDCS,NLANDGS,NLANDC, NLANDG, RADD,   SADD,
     V           BI, PSISAT, DD, XSLOPE, BULK_FC  )
C
C     * CALCULATIONS FOR CANOPY OVER SNOW.
C
      IF(NLANDCS.GT.0)                                              THEN
          CALL CANVAP(EVAPCS,SUBLCS,RAICNS,SNOCNS,TCANS,THLQCS,
     1                TBARCS,ZSNOCS,WLSTCS,CHCAPS,QFCF,QFCL,QFN,QFC,
     2                HTCC,HTCS,HTC,FCS,CMASCS,TSNOCS,HCPSCS,RHOSCS,
     3                FROOT,THPOR,THLMIN,DELZW,EVLOST,RLOST,IROOT,
     4                IG,ILG,IL1,IL2,JL,N  )
          CALL CANADD(2,RPCCS,TRPCCS,SPCCS,TSPCCS,RAICNS,SNOCNS,
     1                TCANS,CHCAPS,HTCC,ROFC,ROVG,PCPN,PCPG,
     2                FCS,FSVFS,CWLCPS,CWFCPS,CMASCS,RHOSNI,
     3                TSFSAV(1,1),RADD,SADD,ILG,IL1,IL2,JL)
          CALL CWCALC(TCANS,RAICNS,SNOCNS,RDUMMY,RDUMMY,CHCAPS,
     1                HMFC,HTCC,FCS,CMASCS,ILG,IL1,IL2,JL)
          CALL SUBCAN(2,RPCCS,TRPCCS,SPCCS,TSPCCS,RHOSNI,EVPCSG,
     1                QFN,QFG,PCPN,PCPG,FCS,ILG,IL1,IL2,JL)
          CALL TWCALC(TBARCS,THLQCS,THICCS,HCPCS,TBRWCS,HMFG,HTC,
     1                FCS,ZERO,THPOR,THLMIN,HCPS,DELZW,DELZZ,ISAND,
     2                IG,ILG,IL1,IL2,JL)
          CALL SNOVAP(RHOSCS,ZSNOCS,HCPSCS,TSNOCS,EVPCSG,QFN,QFG,
     1                HTCS,WLSTCS,TRNFCS,RUNFCS,TOVRFL,OVRFLW,
     2                FCS,RPCCS,SPCCS,RHOSNI,WSNOCS,ILG,IL1,IL2,JL)
          CALL TFREEZ(ZPNDCS,TPNDCS,ZSNOCS,TSNOCS,ALBSCS,
     1                RHOSCS,HCPSCS,GZROCS,HMFG,HTCS,HTC,
     2                WTRS,WTRG,FCS,ZERO,WSNOCS,TA,TBARCS,
     3                ISAND,IG,ILG,IL1,IL2,JL) 
          CALL TMELT(ZSNOCS,TSNOCS,QMELTC,RPCCS,TRPCCS,
     1               GZROCS,RALB,HMFN,HTCS,HTC,FCS,HCPSCS,
     2               RHOSCS,WSNOCS,ISAND,IG,ILG,IL1,IL2,JL)
          CALL SNOADD(ALBSCS,TSNOCS,RHOSCS,ZSNOCS,
     1                HCPSCS,HTCS,FCS,SPCCS,TSPCCS,RHOSNI,WSNOCS,
     2                ILG,IL1,IL2,JL)
          IF(FROZENSOILINFILFLAG.GE.1)THEN
             CALL SNINFLM(RPCCS,TRPCCS,ZSNOCS,TSNOCS,RHOSCS,HCPSCS,
     1                    WSNOCS,HTCS,HMFN,PCPG,ROFN,FCS,ILG,IL1,IL2,JL,
     2                    NCOUNT,RUNFCS,TRNFCS,OVRFLW,TOVRFL,
     3                    SOIL_POR_MAX,SOIL_DEPTH,S0,T_ICE_LENS,
     3                    THLIQ(:,1)+THICE(:,1),TBARCS(:,1),
     4                    t0_ACC,SI,TSI,INFILTYPE,
     5                    SNOWMELTD,SNOWMELTD_LAST,MELTRUNOFF,
     6                    SNOWINFIL,CUMSNOWINFILCS,FRZC,
     7                    FROZENSOILINFILFLAG,ZPNDCS,TPNDCS)
          ELSE
             CALL SNINFL(RPCCS,TRPCCS,ZSNOCS,TSNOCS,RHOSCS,HCPSCS,
     1                   WSNOCS,HTCS,HMFN,PCPG,ROFN,FCS,ILG,IL1,IL2,JL)
          ENDIF
          CALL GRINFL(1,THLQCS,THICCS,TBRWCS,BASFLW,TBASFL,RUNFCS,
     1                TRNFCS,ZFAV,LZFAV,THLINV,QFG,WLSTCS,
     2                FCS,EVPCSG,RPCCS,TRPCCS,TPNDCS,ZPNDCS,
     3                DT,ZMAT,WMOVE,TMOVE,THLIQX,THICEX,TBARWX,
     4                DELZX,ZBOTX,FDT,TFDT,PSIF,THLINF,GRKINF,
     5                THLMAX,THTEST,ZRMDR,FDUMMY,TDUMMY,THLDUM,
     6                THIDUM,TDUMW,TRMDR,ZF,FMAX,TUSED,RDUMMY,
     7                ZERO,WEXCES,FDTBND,WADD,TADD,WADJ,TIMPND,
     8                DZF,DTFLOW,THLNLZ,THLQLZ,DZDISP,WDISP,WABS,
     9                THPOR,THLRET,THLMIN,BI,PSISAT,GRKSCS,
     A                THLRAT,THFC,DELZW,ZBOTW,XDRAIN,DELZ,ISAND,
     B                IGRN,IGRD,IFILL,IZERO,LZF,NINF,IFIND,ITER,
     C                NEND,ISIMP,IGDR,
     D                IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
          CALL GRDRAN(1,THLQCS,THICCS,TBRWCS,FDUMMY,TDUMMY,
     1                BASFLW,TBASFL,RUNFCS,TRNFCS,
     2                QFG,WLSTCS,FCS,EVPCSG,RPCCS,ZPNDCS,
     3                DT,WEXCES,THLMAX,THTEST,THPOR,THLRET,THLMIN,
     4                BI,PSISAT,GRKSCS,THFC,DELZW,XDRAIN,ISAND,
     5                IZERO,IGRN,IGRD,IGDR,
     6                IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
          CALL PDMROF(IWF, ILG, IL1, IL2, FCS,
     1                ZPNDPRECS, ZPNDCS, FSTRCS, TPNDCS,
     2                OVRFLW, TOVRFL, RUNFCS, TRNFCS, TFREZ,
     3                CMIN, CMAX, B, K1, K2,
     4                UM1CS, QM1CS, QM2CS, UMQCS, DELT)
          CALL LATFLOW(THLQCS, THICCS, TPNDCS, OVRFLW, TOVRFL,
     1                 SUBFLW, TSUBFL, RUNFCS, TRNFCS, FCS, ZPLMCS,
     2                 XSLOPE, XDRAINH, MANNING_N, DD, KSAT, TBRWCS,
     3                 DELZW, THPOR, THLMIN, BI, DIDRN,
     4                 ISAND, IWF, IG, ILG, IL1, IL2, BULK_FC,
     5                 ZPNDPRECS, ZPNDCS, FSTRCS, CMIN, CMAX, B,
     6                 CSTRCS, UMQCS)
          CALL WATROF(THLQCS, THICCS, ZPNDCS, TPNDCS, OVRFLW, TOVRFL,
     1                SUBFLW, TSUBFL, RUNFCS, TRNFCS, FCS, ZPLMCS,
     2                XSLOPE, XDRAINH, MANNING_N, DD, KSAT, TBRWCS,
     3                DELZW, THPOR, THLMIN, BI, DODRN, DOVER, DIDRN,
     4                ISAND, IWF, IG, ILG, IL1, IL2, BULK_FC)
          CALL TMCALC(TBARCS,THLQCS,THICCS,HCPCS,TPNDCS,ZPNDCS,
     1                TSNOCS,ZSNOCS,ALBSCS,RHOSCS,HCPSCS,TBASCS,
     2                OVRFLW,TOVRFL,RUNFCS,TRNFCS,HMFG,HTC,HTCS,
     3                WTRS,WTRG,FCS,TBRWCS,GZROCS,G12CS,
     4                G23CS,GGEO,TA,WSNOCS,TCTOPC,TCBOTC,GFLXCS,
     5                ZPLMCS,THPOR,THLMIN,HCPS,DELZW,DELZZ,DELZ,
     6                ISAND,IWF,IG,ILG,IL1,IL2,JL,N)
          CALL CHKWAT(1,PCPR,EVPICS,RUNFCS,WLSTCS,RAICNS,SNOCNS,
     1                RACS,SNCS,ZPNDCS,ZPOND,THLQCS,THICCS,
     2                THLIQC,THICEC,ZSNOCS,RHOSCS,XSNOCS,SNO,
     3                WSNOCS,WSNOW,FCS,FGS,FCS,BAL,THPOR,THLMIN,
     4                DELZW,ISAND,IG,ILG,IL1,IL2,JL,N   ) 
          CALL SNOALBW(ALBSCS,RHOSCS,ZSNOCS,HCPSCS,
     1                 TSNOCS,FCS,SPCCS,RALB,WSNOCS,RHOMAX,
     2                 ISAND,ILG,IG,IL1,IL2,JL)       
      ENDIF                                                               
C
C     * CALCULATIONS FOR SNOW-COVERED GROUND.
C
      IF(NLANDGS.GT.0)                                              THEN
          CALL TWCALC(TBARGS,THLQGS,THICGS,HCPGS,TBRWGS,HMFG,HTC,
     1                FGS,ZERO,THPOR,THLMIN,HCPS,DELZW,DELZZ,ISAND,
     2                IG,ILG,IL1,IL2,JL)
          CALL SNOVAP(RHOSGS,ZSNOGS,HCPSGS,TSNOGS,EVAPGS,QFN,QFG,
     1                HTCS,WLSTGS,TRNFGS,RUNFGS,TOVRFL,OVRFLW,
     2                FGS,RPCGS,SPCGS,RHOSNI,WSNOGS,ILG,IL1,IL2,JL)  
          CALL TFREEZ(ZPNDGS,TPNDGS,ZSNOGS,TSNOGS,ALBSGS,
     1                RHOSGS,HCPSGS,GZROGS,HMFG,HTCS,HTC,
     2                WTRS,WTRG,FGS,ZERO,WSNOGS,TA,TBARGS,
     3                ISAND,IG,ILG,IL1,IL2,JL) 
          CALL TMELT(ZSNOGS,TSNOGS,QMELTG,RPCGS,TRPCGS,
     1               GZROGS,RALB,HMFN,HTCS,HTC,FGS,HCPSGS,
     2               RHOSGS,WSNOGS,ISAND,IG,ILG,IL1,IL2,JL)
          CALL SNOADD(ALBSGS,TSNOGS,RHOSGS,ZSNOGS,
     1                HCPSGS,HTCS,FGS,SPCGS,TSPCGS,RHOSNI,WSNOGS,
     2                ILG,IL1,IL2,JL)
          IF(FROZENSOILINFILFLAG.GE.1)THEN
             CALL SNINFLM(RPCGS,TRPCGS,ZSNOGS,TSNOGS,RHOSGS,HCPSGS,
     1                    WSNOGS,HTCS,HMFN,PCPG,ROFN,FGS,ILG,IL1,IL2,JL,
     2                    NCOUNT,RUNFGS,TRNFGS,OVRFLW,TOVRFL,
     3                    SOIL_POR_MAX,SOIL_DEPTH,S0,T_ICE_LENS,
     3                    THLIQ(:,1)+THICE(:,1),TBARGS(:,1),
     4                    t0_ACC,SI,TSI,INFILTYPE,
     5                    SNOWMELTD,SNOWMELTD_LAST,MELTRUNOFF,
     6                    SNOWINFIL,CUMSNOWINFILGS,FRZC,
     7                    FROZENSOILINFILFLAG,ZPNDGS,TPNDGS)
          ELSE
             CALL SNINFL(RPCGS,TRPCGS,ZSNOGS,TSNOGS,RHOSGS,HCPSGS,
     1                   WSNOGS,HTCS,HMFN,PCPG,ROFN,FGS,ILG,IL1,IL2,JL)
          ENDIF
          
          IF(NLANDI.NE.0)                                       THEN
              CALL ICEBAL(TBARGS,TPNDGS,ZPNDGS,TSNOGS,RHOSGS,ZSNOGS,
     1                    HCPSGS,ALBSGS,HMFG,HTCS,HTC,WTRS,WTRG,GFLXGS,
     2                    RUNFGS,TRNFGS,OVRFLW,TOVRFL,ZPLMGS,GGEO,
     3                    FGS,EVAPGS,RPCGS,TRPCGS,GZROGS,G12GS,G23GS,
     4                    HCPGS,QMELTG,WSNOGS,ZMAT,TMOVE,WMOVE,ZRMDR,
     5                    TADD,ZMOVE,TBOT,DELZ,ISAND,ICONT,
     6                    IWF,IG,IGP1,IGP2,ILG,IL1,IL2,JL,N )
          ENDIF
          CALL GRINFL(2,THLQGS,THICGS,TBRWGS,BASFLW,TBASFL,RUNFGS,
     1                TRNFGS,ZFAV,LZFAV,THLINV,QFG,WLSTGS,
     2                FGS,EVAPGS,RPCGS,TRPCGS,TPNDGS,ZPNDGS,
     3                DT,ZMAT,WMOVE,TMOVE,THLIQX,THICEX,TBARWX,
     4                DELZX,ZBOTX,FDT,TFDT,PSIF,THLINF,GRKINF,
     5                THLMAX,THTEST,ZRMDR,FDUMMY,TDUMMY,THLDUM,
     6                THIDUM,TDUMW,TRMDR,ZF,FMAX,TUSED,RDUMMY,
     7                ZERO,WEXCES,FDTBND,WADD,TADD,WADJ,TIMPND,
     8                DZF,DTFLOW,THLNLZ,THLQLZ,DZDISP,WDISP,WABS,
     9                THPOR,THLRET,THLMIN,BI,PSISAT,GRKSGS,
     A                THLRAT,THFC,DELZW,ZBOTW,XDRAIN,DELZ,ISAND,
     B                IGRN,IGRD,IFILL,IZERO,LZF,NINF,IFIND,ITER,
     C                NEND,ISIMP,IGDR,
     D                IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
          CALL GRDRAN(2,THLQGS,THICGS,TBRWGS,FDUMMY,TDUMMY,
     1                BASFLW,TBASFL,RUNFGS,TRNFGS,
     2                QFG,WLSTGS,FGS,EVAPGS,RPCGS,ZPNDGS,
     3                DT,WEXCES,THLMAX,THTEST,THPOR,THLRET,THLMIN,
     4                BI,PSISAT,GRKSGS,THFC,DELZW,XDRAIN,ISAND,
     5                IZERO,IGRN,IGRD,IGDR,
     6                IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
          CALL PDMROF(IWF, ILG, IL1, IL2, FGS,
     1                ZPNDPREGS, ZPNDGS, FSTRGS, TPNDGS,
     2                OVRFLW, TOVRFL, RUNFGS, TRNFGS, TFREZ,
     3                CMIN, CMAX, B, K1, K2,
     4                UM1GS, QM1GS, QM2GS, UMQGS, DELT)
          CALL LATFLOW(THLQGS, THICGS, TPNDGS, OVRFLW, TOVRFL,
     1                 SUBFLW, TSUBFL, RUNFGS, TRNFGS, FGS, ZPLMGS,
     2                 XSLOPE, XDRAINH, MANNING_N, DD, KSAT, TBRWGS,
     3                 DELZW, THPOR, THLMIN, BI, DIDRN,
     4                 ISAND, IWF, IG, ILG, IL1, IL2, BULK_FC,
     5                 ZPNDPREGS, ZPNDGS, FSTRGS, CMIN, CMAX, B,
     6                 CSTRGS, UMQGS)
          CALL WATROF(THLQGS, THICGS, ZPNDGS, TPNDGS, OVRFLW, TOVRFL,
     1                SUBFLW, TSUBFL, RUNFGS, TRNFGS, FGS, ZPLMGS,
     2                XSLOPE, XDRAINH, MANNING_N, DD, KSAT, TBRWGS,
     3                DELZW, THPOR, THLMIN, BI, DODRN, DOVER, DIDRN,
     4                ISAND, IWF, IG, ILG, IL1, IL2, BULK_FC)
          CALL TMCALC(TBARGS,THLQGS,THICGS,HCPGS,TPNDGS,ZPNDGS,
     1                TSNOGS,ZSNOGS,ALBSGS,RHOSGS,HCPSGS,TBASGS,
     2                OVRFLW,TOVRFL,RUNFGS,TRNFGS,HMFG,HTC,HTCS,
     3                WTRS,WTRG,FGS,TBRWGS,GZROGS,G12GS,
     4                G23GS,GGEO,TA,WSNOGS,TCTOPG,TCBOTG,GFLXGS,
     5                ZPLMGS,THPOR,THLMIN,HCPS,DELZW,DELZZ,DELZ,
     6                ISAND,IWF,IG,ILG,IL1,IL2,JL,N)
          CALL CHKWAT(2,PCPR,EVPIGS,RUNFGS,WLSTGS,RAICNS,SNOCNS,
     1                RACS,SNCS,ZPNDGS,ZPOND,THLQGS,THICGS,
     2                THLIQG,THICEG,ZSNOGS,RHOSGS,XSNOGS,SNO,
     3                WSNOGS,WSNOW,FCS,FGS,FGS,BAL,THPOR,THLMIN,
     4                DELZW,ISAND,IG,ILG,IL1,IL2,JL,N   ) 
          CALL SNOALBW(ALBSGS,RHOSGS,ZSNOGS,HCPSGS,
     1                 TSNOGS,FGS,SPCGS,RALB,WSNOGS,RHOMAX,
     2                 ISAND,ILG,IG,IL1,IL2,JL)       
      ENDIF                                                               
C
C     * CALCULATIONS FOR CANOPY OVER BARE GROUND.
C
      IF(NLANDC.GT.0)                                               THEN
          CALL CANVAP(EVAPC,SUBLC,RAICAN,SNOCAN,TCANO,THLQCO,
     1                TBARC,ZSNOWC,WLOSTC,CHCAP,QFCF,QFCL,QFN,QFC,
     2                HTCC,HTCS,HTC,FC,CMASSC,TSNOWC,HCPSC,RHOSC,
     3                FROOT,THPOR,THLMIN,DELZW,EVLOST,RLOST,IROOT,
     4                IG,ILG,IL1,IL2,JL,N  )
          CALL CANADD(1,RPCC,TRPCC,SPCC,TSPCC,RAICAN,SNOCAN,
     1                TCANO,CHCAP,HTCC,ROFC,ROVG,PCPN,PCPG,
     2                FC,FSVF,CWLCAP,CWFCAP,CMASSC,RHOSNI,
     3                TSFSAV(1,3),RADD,SADD,ILG,IL1,IL2,JL)     
          CALL CWCALC(TCANO,RAICAN,SNOCAN,RDUMMY,RDUMMY,CHCAP,
     1                HMFC,HTCC,FC,CMASSC,ILG,IL1,IL2,JL)
          CALL SUBCAN(1,RPCC,TRPCC,SPCC,TSPCC,RHOSNI,EVAPCG,
     1                QFN,QFG,PCPN,PCPG,FC,ILG,IL1,IL2,JL)
          CALL TWCALC(TBARC,THLQCO,THICCO,HCPCO,TBARWC,HMFG,HTC,
     1                FC,EVAPCG,THPOR,THLMIN,HCPS,DELZW,DELZZ,
     2                ISAND,IG,ILG,IL1,IL2,JL)
          CALL SNOVAP(RHOSC,ZSNOWC,HCPSC,TSNOWC,EVAPCG,QFN,QFG,
     1                HTCS,WLOSTC,TRUNFC,RUNFC,TOVRFL,OVRFLW,
     2                FC,RPCC,SPCC,RHOSNI,ZERO,ILG,IL1,IL2,JL)
          CALL TFREEZ(ZPONDC,TPONDC,ZSNOWC,TSNOWC,ALBSC,
     1                RHOSC,HCPSC,GZEROC,HMFG,HTCS,HTC,
     2                WTRS,WTRG,FC,QFREZC,ZERO,TA,TBARC,
     3                ISAND,IG,ILG,IL1,IL2,JL) 
          CALL SNOADD(ALBSC,TSNOWC,RHOSC,ZSNOWC,
     1                HCPSC,HTCS,FC,SPCC,TSPCC,RHOSNI,ZERO,
     2                ILG,IL1,IL2,JL)
          CALL GRINFL(3,THLQCO,THICCO,TBARWC,BASFLW,TBASFL,RUNFC,
     1                TRUNFC,ZFAV,LZFAV,THLINV,QFG,WLOSTC,
     2                FC,EVAPCG,RPCC,TRPCC,TPONDC,ZPONDC,
     3                DT,ZMAT,WMOVE,TMOVE,THLIQX,THICEX,TBARWX,
     4                DELZX,ZBOTX,FDT,TFDT,PSIF,THLINF,GRKINF,
     5                THLMAX,THTEST,ZRMDR,FDUMMY,TDUMMY,THLDUM,
     6                THIDUM,TDUMW,TRMDR,ZF,FMAX,TUSED,RDUMMY,
     7                ZERO,WEXCES,FDTBND,WADD,TADD,WADJ,TIMPND,
     8                DZF,DTFLOW,THLNLZ,THLQLZ,DZDISP,WDISP,WABS,
     9                THPOR,THLRET,THLMIN,BI,PSISAT,GRKSC,
     A                THLRAT,THFC,DELZW,ZBOTW,XDRAIN,DELZ,ISAND,
     B                IGRN,IGRD,IFILL,IZERO,LZF,NINF,IFIND,ITER,
     C                NEND,ISIMP,IGDR,
     D                IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
          CALL GRDRAN(3,THLQCO,THICCO,TBARWC,FDUMMY,TDUMMY,
     1                BASFLW,TBASFL,RUNFC,TRUNFC,
     2                QFG,WLOSTC,FC,EVAPCG,RPCC,ZPONDC,
     3                DT,WEXCES,THLMAX,THTEST,THPOR,THLRET,THLMIN,
     4                BI,PSISAT,GRKSC,THFC,DELZW,XDRAIN,ISAND,
     5                IZERO,IGRN,IGRD,IGDR,
     6                IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
          CALL PDMROF(IWF, ILG, IL1, IL2, FC,
     1                ZPONDPREC, ZPONDC, FSTRC, TPONDC,
     2                OVRFLW, TOVRFL, RUNFC, TRUNFC, TFREZ,
     3                CMIN, CMAX, B, K1, K2,
     4                UM1C, QM1C, QM2C, UMQC, DELT)
          CALL LATFLOW(THLQCO, THICCO, ZPONDC, TPONDC, OVRFLW, TOVRFL,
     1                 SUBFLW, TSUBFL, RUNFC, TRUNFC, FC, ZPLIMC,
     2                 XSLOPE, XDRAINH, MANNING_N, DD, KSAT, TBARWC,
     3                 DELZW, THPOR, THLMIN, BI, DIDRN,
     4                 ISAND, IWF, IG, ILG, IL1, IL2, BULK_FC,
     5                 ZPONDPREC, ZPONDC, FSTRC, CMIN, CMAX, B,
     6                 CSTRC, UMQC)
          CALL WATROF(THLQCO, THICCO, ZPONDC, TPONDC, OVRFLW, TOVRFL,
     1                SUBFLW, TSUBFL, RUNFC, TRUNFC, FC, ZPLIMC,
     2                XSLOPE, XDRAINH, MANNING_N, DD, KSAT, TBARWC,
     3                DELZW, THPOR, THLMIN, BI, DODRN, DOVER, DIDRN,
     4                ISAND, IWF, IG, ILG, IL1, IL2, BULK_FC)
          CALL TMCALC(TBARC,THLQCO,THICCO,HCPCO,TPONDC,ZPONDC,
     1                TSNOWC,ZSNOWC,ALBSC,RHOSC,HCPSC,TBASC,
     2                OVRFLW,TOVRFL,RUNFC,TRUNFC,HMFG,HTC,HTCS,
     3                WTRS,WTRG,FC,TBARWC,GZEROC,G12C,
     4                G23C,GGEO,TA,ZERO,TCTOPC,TCBOTC,GFLXC,
     5                ZPLIMC,THPOR,THLMIN,HCPS,DELZW,DELZZ,DELZ,
     6                ISAND,IWF,IG,ILG,IL1,IL2,JL,N)
          CALL CHKWAT(3,PCPR,EVPIC,RUNFC,WLOSTC,RAICAN,SNOCAN,
     1                RAC,SNC,ZPONDC,ZPOND,THLQCO,THICCO,
     2                THLIQC,THICEC,ZSNOWC,RHOSC,XSNOWC,SNO,
     3                ZERO,ZERO,FCS,FGS,FC,BAL,THPOR,THLMIN,
     4                DELZW,ISAND,IG,ILG,IL1,IL2,JL,N    ) 
C
      ENDIF                                                               
C
C     * CALCULATIONS FOR BARE GROUND.
C
      IF(NLANDG.GT.0)                                               THEN
          CALL TWCALC(TBARG,THLQGO,THICGO,HCPGO,TBARWG,HMFG,HTC,
     1                FG,EVAPG,THPOR,THLMIN,HCPS,DELZW,DELZZ,
     2                ISAND,IG,ILG,IL1,IL2,JL)             
          CALL SNOVAP(RHOSG,ZSNOWG,HCPSG,TSNOWG,EVAPG,QFN,QFG,
     1                HTCS,WLOSTG,TRUNFG,RUNFG,TOVRFL,OVRFLW,
     2                FG,RPCG,SPCG,RHOSNI,ZERO,ILG,IL1,IL2,JL)
          CALL TFREEZ(ZPONDG,TPONDG,ZSNOWG,TSNOWG,ALBSG,
     1                RHOSG,HCPSG,GZEROG,HMFG,HTCS,HTC,
     2                WTRS,WTRG,FG,QFREZG,ZERO,TA,TBARG,
     3                ISAND,IG,ILG,IL1,IL2,JL) 
          CALL SNOADD(ALBSG,TSNOWG,RHOSG,ZSNOWG,
     1                HCPSG,HTCS,FG,SPCG,TSPCG,RHOSNI,ZERO,
     2                ILG,IL1,IL2,JL)
          IF(NLANDI.NE.0)                                       THEN
              CALL ICEBAL(TBARG,TPONDG,ZPONDG,TSNOWG,RHOSG,ZSNOWG,
     1                    HCPSG,ALBSG,HMFG,HTCS,HTC,WTRS,WTRG,GFLXG,
     2                    RUNFG,TRUNFG,OVRFLW,TOVRFL,ZPLIMG,GGEO,
     3                    FG,EVAPG,RPCG,TRPCG,GZEROG,G12G,G23G,
     4                    HCPGO,QFREZG,ZERO,ZMAT,TMOVE,WMOVE,ZRMDR,
     5                    TADD,ZMOVE,TBOT,DELZ,ISAND,ICONT,
     6                    IWF,IG,IGP1,IGP2,ILG,IL1,IL2,JL,N )
          ENDIF
          CALL GRINFL(4,THLQGO,THICGO,TBARWG,BASFLW,TBASFL,RUNFG,
     1                TRUNFG,ZFAV,LZFAV,THLINV,QFG,WLOSTG,
     2                FG,EVAPG,RPCG,TRPCG,TPONDG,ZPONDG,
     3                DT,ZMAT,WMOVE,TMOVE,THLIQX,THICEX,TBARWX,
     4                DELZX,ZBOTX,FDT,TFDT,PSIF,THLINF,GRKINF,
     5                THLMAX,THTEST,ZRMDR,FDUMMY,TDUMMY,THLDUM,
     6                THIDUM,TDUMW,TRMDR,ZF,FMAX,TUSED,RDUMMY,
     7                ZERO,WEXCES,FDTBND,WADD,TADD,WADJ,TIMPND,
     8                DZF,DTFLOW,THLNLZ,THLQLZ,DZDISP,WDISP,WABS,
     9                THPOR,THLRET,THLMIN,BI,PSISAT,GRKSG,
     A                THLRAT,THFC,DELZW,ZBOTW,XDRAIN,DELZ,ISAND,
     B                IGRN,IGRD,IFILL,IZERO,LZF,NINF,IFIND,ITER,
     C                NEND,ISIMP,IGDR,
     D                IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
          CALL GRDRAN(4,THLQGO,THICGO,TBARWG,FDUMMY,TDUMMY,
     1                BASFLW,TBASFL,RUNFG,TRUNFG,
     2                QFG,WLOSTG,FG,EVAPG,RPCG,ZPONDG,
     3                DT,WEXCES,THLMAX,THTEST,THPOR,THLRET,THLMIN,
     4                BI,PSISAT,GRKSG,THFC,DELZW,XDRAIN,ISAND,
     5                IZERO,IGRN,IGRD,IGDR,
     6                IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
          CALL PDMROF(IWF, ILG, IL1, IL2, FG,
     1                ZPONDPREG, ZPONDG, FSTRG, TPONDG,
     2                OVRFLW, TOVRFL, RUNFG, TRUNFG, TFREZ,
     3                CMIN, CMAX, B, K1, K2,
     4                UM1G, QM1G, QM2G, UMQG, DELT)
          CALL LATFLOW(THLQGO, THICGO, ZPONDG, TPONDG, OVRFLW, TOVRFL,
     1                 SUBFLW, TSUBFL, RUNFG, TRUNFG, FG, ZPLIMG,
     2                 XSLOPE, XDRAINH, MANNING_N, DD, KSAT, TBARWG,
     3                 DELZW, THPOR, THLMIN, BI, DIDRN,
     4                 ISAND, IWF, IG, ILG, IL1, IL2, BULK_FC,
     5                 ZPONDPREG, ZPONDG, FSTRG, CMIN, CMAX, B,
     6                 CSTRG, UMQG)
          CALL WATROF(THLQGO, THICGO, ZPONDG, TPONDG, OVRFLW, TOVRFL,
     1                SUBFLW, TSUBFL, RUNFG, TRUNFG, FG, ZPLIMG,
     2                XSLOPE, XDRAINH, MANNING_N, DD, KSAT, TBARWG,
     3                DELZW, THPOR, THLMIN, BI, DODRN, DOVER, DIDRN,
     4                ISAND, IWF, IG, ILG, IL1, IL2, BULK_FC)
          CALL TMCALC(TBARG,THLQGO,THICGO,HCPGO,TPONDG,ZPONDG,
     1                TSNOWG,ZSNOWG,ALBSG,RHOSG,HCPSG,TBASG,
     2                OVRFLW,TOVRFL,RUNFG,TRUNFG,HMFG,HTC,HTCS,
     3                WTRS,WTRG,FG,TBARWG,GZEROG,G12G,
     4                G23G,GGEO,TA,ZERO,TCTOPG,TCBOTG,GFLXG,
     5                ZPLIMG,THPOR,THLMIN,HCPS,DELZW,DELZZ,DELZ,
     6                ISAND,IWF,IG,ILG,IL1,IL2,JL,N)
          CALL CHKWAT(4,PCPR,EVPIG,RUNFG,WLOSTG,RAICAN,SNOCAN,
     1                RAC,SNC,ZPONDG,ZPOND,THLQGO,THICGO,
     2                THLIQG,THICEG,ZSNOWG,RHOSG,XSNOWG,SNO,
     3                ZERO,ZERO,FCS,FGS,FG,BAL,THPOR,THLMIN,
     4                DELZW,ISAND,IG,ILG,IL1,IL2,JL,N   ) 
C
      ENDIF
C
C     * AVERAGE RUNOFF AND PROGNOSTIC VARIABLES OVER FOUR GRID CELL
C     * SUBAREAS.
C
      JPTBAD=0
      KPTBAD=0
      LPTBAD=0
      !$omp parallel do
      DO 600 I=IL1,IL2 
          TBASE (I)=FCS(I)*(TBASCS(I)+TFREZ) + 
     1              FGS(I)*(TBASGS(I)+TFREZ) +
     2              FC (I)*(TBASC (I)+TFREZ) + 
     3              FG (I)*(TBASG (I)+TFREZ)
          RUNOFF(I)=FCS(I)*RUNFCS(I) + FGS(I)*RUNFGS(I) +
     1              FC (I)*RUNFC (I) + FG (I)*RUNFG (I)
          UMQ(I)=FCS(I)*UMQCS(I) + FGS(I)*UMQGS(I) +
     1              FC (I)*UMQC (I) + FG (I)*UMQG (I)
          IF(RUNOFF(I).GT.0.0) 
     1        TRUNOF(I)=(FCS(I)*RUNFCS(I)*TRNFCS(I) + 
     2                   FGS(I)*RUNFGS(I)*TRNFGS(I) +
     3                   FC (I)*RUNFC (I)*TRUNFC(I) + 
     4                   FG (I)*RUNFG (I)*TRUNFG(I))/RUNOFF(I) 
          RUNOFF(I)=RUNOFF(I)*RHOW/DELT
          UMQ(I)   = UMQ(I)*RHOW/DELT                           
          OVRFLW(I)=OVRFLW(I)*RHOW/DELT
          SUBFLW(I)=SUBFLW(I)*RHOW/DELT
          BASFLW(I)=BASFLW(I)*RHOW/DELT
          EVAP  (I)=EVAP(I)-(FCS(I)*WLSTCS(I)+FGS(I)*WLSTGS(I)+
     1              FC(I)*WLOSTC(I)+FG(I)*WLOSTG(I))/DELT
          IF((FC(I)+FCS(I)).GT.0.)                                  THEN
              TCAN(I)=(FCS(I)*TCANS(I)*CHCAPS(I)+FC(I)*TCANO(I)*              
     1                CHCAP(I))/(FCS(I)*CHCAPS(I)+FC(I)*CHCAP(I))                 
              RCAN(I)= FCS(I)*RAICNS(I) + FC (I)*RAICAN(I)                            
              IF(TCAN(I).LT.173.16 .OR. TCAN(I).GT.373.16) JPTBAD=I
              IF(RCAN(I).LT.0.0) RCAN(I)=0.0
              IF(RCAN(I).LT.1.0E-5 .AND. RCAN(I).GT.0.0) THEN
                  TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+TCAN(I)*RCAN(I)/
     1                DELT)/(OVRFLW(I)+RCAN(I)/DELT)
                  OVRFLW(I)=OVRFLW(I)+RCAN(I)/DELT
                  TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+TCAN(I)*RCAN(I)/
     1                DELT)/(RUNOFF(I)+RCAN(I)/DELT)
                  RUNOFF(I)=RUNOFF(I)+RCAN(I)/DELT
                  ROFC(I)=ROFC(I)+RCAN(I)/DELT
                  ROVG(I)=ROVG(I)+RCAN(I)/DELT
                  PCPG(I)=PCPG(I)+RCAN(I)/DELT
                  HTCC(I)=HTCC(I)-TCAN(I)*SPHW*RCAN(I)/DELT
                  RCAN(I)=0.0
              ENDIF
              SNCAN  (I)=FCS(I)*SNOCNS(I) + FC (I)*SNOCAN(I)                            
              IF(SNCAN(I).LT.0.0) SNCAN(I)=0.0
              IF(SNCAN(I).LT.1.0E-5 .AND. SNCAN(I).GT.0.0) THEN
                  TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+TCAN(I)*SNCAN(I)/
     1                DELT)/(OVRFLW(I)+SNCAN(I)/DELT)
                  OVRFLW(I)=OVRFLW(I)+SNCAN(I)/DELT
                  TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+TCAN(I)*SNCAN(I)/
     1                DELT)/(RUNOFF(I)+SNCAN(I)/DELT)
                  RUNOFF(I)=RUNOFF(I)+SNCAN(I)/DELT
                  ROFC(I)=ROFC(I)+SNCAN(I)/DELT
                  ROVG(I)=ROVG(I)+SNCAN(I)/DELT
                  PCPG(I)=PCPG(I)+SNCAN(I)/DELT
                  HTCC(I)=HTCC(I)-TCAN(I)*SPHICE*SNCAN(I)/DELT
                  SNCAN(I)=0.0
              ENDIF
          ELSE                                                                
              TCAN(I)=0.0
          ENDIF                                                               
          IF(ZPNDCS(I).GT.0. .OR. ZPNDGS(I).GT.0. .OR.
     1                ZPONDC(I).GT.0. .OR. ZPONDG(I).GT.0.)    THEN 
              ZPOND(I)=(FCS(I)*ZPNDCS(I)+FGS(I)*ZPNDGS(I)+
     1                  FC (I)*ZPONDC(I)+FG (I)*ZPONDG(I))
              TPOND(I)=(FCS(I)*(TPNDCS(I)+TFREZ)*ZPNDCS(I)+
     1                  FGS(I)*(TPNDGS(I)+TFREZ)*ZPNDGS(I)+
     2                  FC (I)*(TPONDC(I)+TFREZ)*ZPONDC(I)+
     3                  FG (I)*(TPONDG(I)+TFREZ)*ZPONDG(I))/
     4                  ZPOND(I)
              IF(ZPOND(I).LT.0.0) ZPOND(I)=0.0
              IF(ZPOND(I).LT.1.0E-8 .AND. ZPOND(I).GT.0.0) THEN
                  TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+TPOND(I)*RHOW*
     1                ZPOND(I)/DELT)/(OVRFLW(I)+RHOW*ZPOND(I)/DELT)
                  OVRFLW(I)=OVRFLW(I)+RHOW*ZPOND(I)/DELT
                  TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+TPOND(I)*RHOW*
     1                ZPOND(I)/DELT)/(RUNOFF(I)+RHOW*ZPOND(I)/DELT)
                  RUNOFF(I)=RUNOFF(I)+RHOW*ZPOND(I)/DELT
                  HTC(I,1)=HTC(I,1)-TPOND(I)*HCPW*ZPOND(I)/DELT
                  TPOND(I)=0.0
                  ZPOND(I)=0.0                            
              ENDIF
         ELSE
              ZPOND(I)=0.0
              TPOND(I)=0.0
         ENDIF
  600 CONTINUE
C
!$omp parallel do
      DO 650 I=IL1,IL2     
          IF(ZSNOCS(I).GT.0. .OR. ZSNOGS(I).GT.0. .OR.
     1       ZSNOWC(I).GT.0. .OR. ZSNOWG(I).GT.0.)              THEN                                             
              IF(ZSNOCS(I).GT.0. .OR. ZSNOGS(I).GT.0.)    THEN                         
                  ALBSNO(I)=(FCS(I)*ALBSCS(I)*XSNOCS(I)+
     1                       FGS(I)*ALBSGS(I)*XSNOGS(I))/
     2                      (FCS(I)*XSNOCS(I)+FGS(I)*XSNOGS(I))                   
              ELSE                                                            
                  ALBSNO(I)=(FC (I)*ALBSC(I)*XSNOWC(I) +
     1                       FG (I)*ALBSG(I)*XSNOWG(I))/
     2                      (FC (I)*XSNOWC(I)+FG (I)*XSNOWG(I))                     
              ENDIF                                                           
              TSNOW(I)=(FCS(I)*(TSNOCS(I)+TFREZ)*HCPSCS(I)*
     1                  ZSNOCS(I)*XSNOCS(I) +                
     2                  FGS(I)*(TSNOGS(I)+TFREZ)*HCPSGS(I)*
     3                  ZSNOGS(I)*XSNOGS(I) +                      
     4                  FC (I)*(TSNOWC(I)+TFREZ)*HCPSC(I)*
     5                  ZSNOWC(I)*XSNOWC(I) +                          
     6                  FG (I)*(TSNOWG(I)+TFREZ)*HCPSG(I)*
     7                  ZSNOWG(I)*XSNOWG(I))/                         
     8                 (FCS(I)*HCPSCS(I)*ZSNOCS(I)*XSNOCS(I) +                               
     9                  FGS(I)*HCPSGS(I)*ZSNOGS(I)*XSNOGS(I) +                                
     A                  FC (I)*HCPSC(I)*ZSNOWC(I)*XSNOWC(I) +                                 
     B                  FG (I)*HCPSG(I)*ZSNOWG(I)*XSNOWG(I))
              RHOSNO(I)=(FCS(I)*RHOSCS(I)*ZSNOCS(I)*XSNOCS(I) +                         
     1                   FGS(I)*RHOSGS(I)*ZSNOGS(I)*XSNOGS(I) +                                
     2                   FC (I)*RHOSC(I)*ZSNOWC(I)*XSNOWC(I) +                                 
     3                   FG (I)*RHOSG(I)*ZSNOWG(I)*XSNOWG(I))/                                
     4                  (FCS(I)*ZSNOCS(I)*XSNOCS(I) +
     5                   FGS(I)*ZSNOGS(I)*XSNOGS(I) +                 
     6                   FC (I)*ZSNOWC(I)*XSNOWC(I) +
     7                   FG (I)*ZSNOWG(I)*XSNOWG(I))                    
              ZSNOW(I)=FCS(I)*ZSNOCS(I) + FGS(I)*ZSNOGS(I) +
     1                 FC (I)*ZSNOWC(I) + FG (I)*ZSNOWG(I)
              WSNOW(I)=FCS(I)*WSNOCS(I) + FGS(I)*WSNOGS(I) 
              SNO(I)=ZSNOW(I)*RHOSNO(I)                                       
              IF(SNO(I).LT.0.0) SNO(I)=0.0

             IF (.not. PBSMFLAG) THEN
              IF(SNO(I).LT.1.0E-2 .AND. SNO(I).GT.0.0) THEN
                  TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+TSNOW(I)*(SNO(I)+
     1                WSNOW(I))/DELT)/(OVRFLW(I)+(SNO(I)+WSNOW(I))/
     2                DELT)
                  OVRFLW(I)=OVRFLW(I)+(SNO(I)+WSNOW(I))/DELT
                  TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+TSNOW(I)*(SNO(I)+
     1                WSNOW(I))/DELT)/(RUNOFF(I)+(SNO(I)+WSNOW(I))/
     2                DELT)
                  RUNOFF(I)=RUNOFF(I)+(SNO(I)+WSNOW(I))/DELT
                  ROFN(I)=ROFN(I)+(SNO(I)+WSNOW(I))/DELT
                  PCPG(I)=PCPG(I)+(SNO(I)+WSNOW(I))/DELT
                  HTCS(I)=HTCS(I)-TSNOW(I)*(SPHICE*SNO(I)+SPHW*
     1                WSNOW(I))/DELT
                  TSNOW(I)=0.0
                  RHOSNO(I)=0.0
                  SNO(I)=0.0                            
                  WSNOW(I)=0.0
              ENDIF
             ENDIF !.not. PBSMFLAG
          ELSE                                                                
              TSNOW(I)=0.0                                                    
              RHOSNO(I)=0.0                                                   
              SNO(I)=0.0                                                      
              WSNOW(I)=0.0
          ENDIF
C
          IF(TSNOW(I).LT.0.0) KPTBAD=I
          IF((TSNOW(I)-TFREZ).GT.1.0E-3) LPTBAD=I
  650 CONTINUE
C
      IF(JPTBAD.NE.0)                                               THEN
          WRITE(6,6625) JPTBAD,JL,TCAN(JPTBAD)
 6625     FORMAT('0AT (I,J)= (',I3,',',I3,'), TCAN = ',F10.5)
          CALL XIT('CLASSW2',-2)
      ENDIF
C
      IF(KPTBAD.NE.0)                                               THEN
          WRITE(6,6626) KPTBAD,JL,TSNOW(KPTBAD)
 6626     FORMAT('0AT (I,J)= (',I3,',',I3,'), TSNOW = ',F10.5)
          CALL XIT('CLASSW2',-3)
      ENDIF
C
      IF(LPTBAD.NE.0)                                               THEN
          WRITE(6,6626) LPTBAD,JL,TSNOW(LPTBAD)
          CALL XIT('CLASSW2',-4)
      ENDIF
C
      IPTBAD=0
      DO 700 J=1,IG
      !$omp parallel do
      DO 700 I=IL1,IL2
          IF(IG.EQ.3. .AND. J.EQ.IG .AND. ISAND(I,1).GT.-4)    THEN
              TBAR(I,J)=((FCS(I)*(TBARCS(I,J)+TFREZ)*HCPCS(I,J) +
     1                   FGS(I)*(TBARGS(I,J)+TFREZ)*HCPGS(I,J) +
     2                   FC (I)*(TBARC (I,J)+TFREZ)*HCPCO(I,J) +             
     3                   FG (I)*(TBARG (I,J)+TFREZ)*HCPGO(I,J))*
     4                   DELZW(I,J)+TBASE(I)*HCPSND*
     5                   (DELZ(J)-DELZW(I,J)))/
     4                  ((FCS(I)*HCPCS(I,J) + FGS(I)*HCPGS(I,J) +
     5                   FC (I)*HCPCO(I,J) + FG (I)*HCPGO(I,J))*
     8                   DELZW(I,J)+HCPSND*(DELZ(J)-DELZW(I,J)))             
          ELSE
              TBAR(I,J)=(FCS(I)*(TBARCS(I,J)+TFREZ)*(DELZW(I,J)*
     1                   HCPCS(I,J)+(DELZ(J)-DELZW(I,J))*HCPSND)+
     2                   FGS(I)*(TBARGS(I,J)+TFREZ)*(DELZW(I,J)*
     3                   HCPGS(I,J)+(DELZ(J)-DELZW(I,J))*HCPSND)+
     4                   FC (I)*(TBARC (I,J)+TFREZ)*(DELZW(I,J)*
     5                   HCPCO(I,J)+(DELZ(J)-DELZW(I,J))*HCPSND)+             
     6                   FG (I)*(TBARG (I,J)+TFREZ)*(DELZW(I,J)*
     7                   HCPGO(I,J)+(DELZ(J)-DELZW(I,J))*HCPSND))/
     8                  (FCS(I)*(DELZW(I,J)*HCPCS(I,J)+
     9                   (DELZ(J)-DELZW(I,J))*HCPSND) + 
     A                   FGS(I)*(DELZW(I,J)*HCPGS(I,J)+
     B                   (DELZ(J)-DELZW(I,J))*HCPSND) +
     C                   FC (I)*(DELZW(I,J)*HCPCO(I,J)+
     D                   (DELZ(J)-DELZW(I,J))*HCPSND) + 
     E                   FG (I)*(DELZW(I,J)*HCPGO(I,J)+
     F                   (DELZ(J)-DELZW(I,J))*HCPSND))              
          ENDIF
          THLIQ(I,J)=FCS(I)*THLQCS(I,J)+FGS(I)*THLQGS(I,J)+
     1               FC (I)*THLQCO(I,J)+FG (I)*THLQGO(I,J)                                   
          THICE(I,J)=FCS(I)*THICCS(I,J)+FGS(I)*THICGS(I,J)+
     1               FC (I)*THICCO(I,J)+FG (I)*THICGO(I,J)
          GFLUX(I,J)=FCS(I)*GFLXCS(I,J)+FGS(I)*GFLXGS(I,J)+
     1               FC (I)*GFLXC (I,J)+FG (I)*GFLXG (I,J)
C     ipy test
C          IF(THLIQ(I,J).GT.THFC(I,J))                               THEN
C              BASFLW(I)=BASFLW(I)+(THLIQ(I,J)-THFC(I,J))*DELZW(I,J)*
C     1            RHOW/DELT
C              RUNOFF(I)=RUNOFF(I)+(THLIQ(I,J)-THFC(I,J))*DELZW(I,J)*
C     1            RHOW/DELT
C              HTC(I,J)=HTC(I,J)-TBAR(I,J)*(THLIQ(I,J)-THFC(I,J))*
C     1            HCPW*DELZW(I,J)/DELT
C              THLIQ(I,J)=THFC(I,J)
C          ENDIF
          IF(TBAR(I,1).LT.173.16 .OR. TBAR(I,1).GT.373.16) IPTBAD=I
  700 CONTINUE                                                            
C
      IF(IPTBAD.NE.0)                                               THEN
          WRITE(6,6600) IPTBAD,JL,TBAR(IPTBAD,1)
 6600     FORMAT('0AT (I,J)= (',I3,',',I3,'), TBAR(1) = ',F10.5)
          CALL XIT('CLASSW2',-1)
      ENDIF
C
      CALL CGROW(GROWTH,TBAR,TA,FC,FCS,ILG,IG,IL1,IL2,JL)
C                                                                                  
      RETURN                                                                      
      END        
