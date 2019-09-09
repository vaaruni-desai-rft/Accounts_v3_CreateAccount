041813******************************************************************
041813* COPYBOOK UPDATES:                                              *
041813* NAME/DTE PACKAGE#   WR/PR/IR#                                  *
041813*----------------------------------------------------------------*
021420* JASON RICHIE - COPIED DEACT2 TO CREATE FOR ACCOUNTS_V3 WEB     *
021420*  WR47841       SERVICE                                         *
111A17* NAVEENA VENKATESH
111A17* 11/11/17 HOST045183 WR42499
111A17*          ADD NEW FIELDS - MULTI CURRENCY IND AND
111A17*          ACCOUNT COMPROMISE   5954
111A17*          MULTI CURRENCY IND   5955
111117* CASEY FALK                                                     *
111117* 11/11/17 HOST044664 WR34862                                    *
111117*          ADD NEW FIELDS TBA-EXEMPT & TBA-EXCLUDED.             *
111216* PRASAD POTHURU                                                 *
111216* 11/12/16 HOST044203 WR41184                                    *
111216*           ADD NEW DOL FIDUCIARY FIELDS                         *
091716* RAM BOMMISETTY
091716* 09/17/16 HOST044031 WR40285
091716*          WR40285- ADD MERGE FIELD TO ACCT API 424
112115* SUDHA SOMASUNDARAM
112115* 11/21/15 HOST042890 WR36075
112115*          WR36075- DEVELOP NATURAL PERSON FLAG AT ACCOUNT AND
112115*          CUSTOMER LEVEL.
051113* AJIT BHASKARAN                                                 *00170037
051113* 05/11/13 HOST038758 WR030546                                   *00170038
051113*          RETIRE ACCT FIELDS                                    *00170039
041813* DAVID COLOMBO                                                  *
041813* 04/18/13 HOST039140 WR030235                                   *
041813*          ADD NEW FIELD - OPTIONS INVESTMENT CODE               *
041813*----------------------------------------------------------------*
      * UPDATES:                                                 *
      * DXH   08/13/11 HOST036032 WR24560 ADD INST-ACCT-IND      *
      * MSG   07/16/11 WR22155- ADD STMT-MMYY & COMM-OVERRIDE-SW *
      * KJB   04/17/10 WR9989 - SUPPORT ACT_ALF MAINTENANCE      *
      * GTE   04/18/09 DEFAULT CRENCY - WR13446                  *
      * JRICH 06/14/08 ADD NEW FLAG TO IDENTIFY CHG UPDATE       *
      * JRICH 04/12/08 ADD OLD PREVENT PRINCIPAL TRADES          *
      * SRJ   10/05/06 ADD OLD REINVEST CODE WR9356              *
      * SRJ   09/21/06 AGE MTB2 - STOCK LOAN AVAILABILITY  WR7188*
      * SRJ   09/21/06 ADD SOCIAL CODE  WR9141                   *
      * RMJ   07/20/06 ADD AED (ACCESS EQUALS DELIVERY) WR6566   *
      * TXL   04/20/06 ADD ALERT FIELDS WR1839                   *
      * SRJ   11/17/05 ADD FRD CNT AND TRD SCHED WR4431          *
      * AJS   04/21/05 ADD FIRM AND SUB  WR816                   *
      * AJS   04/07/05 ADD ADDITIONAL PHONE NUMBERS  92-4451     *
      * SRJ   10/07/04 ADD ACCT OLD PRINCIPAL  34-0876           *
060304* JTF   06/03/04 ADD NEW STATE REGISTRATION FIELD 34-776   *
      * SRJ   04/07/04 ADD NEW DIV INSTRUCTION CODES  34-0770    *
      * AJS   04/07/04 ADD ACCOUNT ACQUIRED  97-509              *
030404* JTF   03/04/04 ADD ACCT-NAV TO DCM_CHG SCR# 34-837       *
      * SRJ   03/03/04 ADD IPO ELIGIBLE INDICATOR  97-0535       *
022804* JTF   02/28/04 ADD NA-AVG-PRICE-INDICATOR SCR# 92-3140   *
022704* JTF   02/27/04 ADD NA-PRODUCT-CLASS DCM_CHG SCR# 34-748  *
      * SRJ   01/22/04 ADD NA-MFXCHG  23-0773                    *
      * DPK   10/16/03 REMOVE HAPI FIELDS.     99-5233           *
      * SRJ   10/02/03 REMOVE ACCT-MMF-ACCOUNT 99-5382           *
092003* JTF   09/20/03 ADD NA-ACCT-STATUS-IND SCR# 92-2783       *
      * CXM   04/17/03 ADD FIELDS FOR ANTI MONEY LAUNDERING      *
      *       97-393   PROJECT.                                  *
      * MPB   03/06/03 ADD BRR FIELD FOR ANTI MONEY LAUNDERING   *
      *       97-393   PROJECT.                                  *
      * NWE   11-28-01 BRS PHASE 1                               *
      * SJH   11/20/01 RENAME BYPASS-REJECT-ACH FIELD TO         *
      *                BYPASS-REJECT-ACAT.  SCR 99-4076.         *
      * SDK   10/04/01  TABLEIZED ALPHA-KEYS                     *
      * MPB   09/06/01 ADD ACCT-WITHHOLD-RYMD FIELD.             *
      *       23-259                                             *
      * SJH   08/17/01 ADD BYPASS-REJECT-ACH FIELD. SCR 27-00219 *
      * MPB   08/16/01 ADD FIELDS FOR MMF3-PROJECT.              *
      ************************************************************
      **  DECOMM COPYBOOK
      ************************************************************
       01  WEB-DEACT2-PARAMETERS.
           04  DE-COMM-AREA.
               05  DE-FIRM               PIC 9(3).
               05  DE-SUB                PIC 9(3).
               05  DE-REQ.
                   10  DE-APPLICATION    PIC X(4).
                       88  APPL-ACCT     VALUE 'ACCT'.
                       88  APPL-FUND     VALUE 'FUND'.
                       88  APPL-SPHD     VALUE 'SPHD'.
100897                 88  APPL-SHIP     VALUE 'SHIP'.
                       88  APPL-IRAS     VALUE 'IRAS'.
                       88  APPL-VALU     VALUE 'VALU'.
                       88  APPL-DESC     VALUE 'DESC'.
060998                 88  APPL-VICE     VALUE 'VICE'.
                       88  APPL-SACH     VALUE 'SACH'.
                       88  APPL-PIPS     VALUE 'PIPS'.
                       88  APPL-DOCA     VALUE 'DOCA'.
012099                 88  APPL-HOLD     VALUE 'HOLD'.
122100                 88  APPL-WRAP     VALUE 'WRAP'.
021501                 88  APPL-BORD     VALUE 'BORD'.
080201                 88  APPL-MADD     VALUE 'MADD'.
032102                 88  APPL-SPAD     VALUE 'SPAD'.
041802                 88  APPL-CUST     VALUE 'CUST'.
071802                 88  APPL-AIRE     VALUE 'AIRE'.
020803                 88  APPL-HAPI     VALUE 'HAPI'.
                   10  DE-FUNCTION       PIC X(4).
                       88  FUNC-ADD      VALUE 'ADD '.
                       88  FUNC-EDIT     VALUE 'EDIT'.
                       88  FUNC-CORR     VALUE 'CORR'.
                       88  FUNC-VOID     VALUE 'VOID'.
                       88  FUNC-INQ      VALUE 'INQ '.
041412                 88  FUNC-FIRST    VALUE 'IFUN'.
                       88  FUNC-FRWD     VALUE 'FRWD'.
                       88  FUNC-LAST     VALUE 'LAST'.
041412                 88  FUNC-CLOSE    VALUE 'CFUN'.
111012                 88  FUNC-WASH-ADD VALUE 'WASH'.
111012                 88  FUNC-WASH-REM VALUE 'WREM'.
               05  DE-USER-INFO.
                   10  DE-SECURITY-LEVEL     PIC XX.
                   10  DE-ERROR-CODE         PIC 9(4).
                   10  DE-USER.
                       15  DE-TERMID         PIC X(10).
                       15  DE-USERID         PIC X(10).
040816                 15  FILLER REDEFINES DE-USERID.
040816                     20  DE-EMPLOYEE-NO PIC 9(5).
040816                     20  FILLER        PIC X(5).
                   10  DE-DEPT               PIC X(4).
                   10  DE-WIRE-CODE          PIC X(4).
               05  DE-MESSAGE                PIC X(25).
               05  DE-ERROR-TO-TERM-SW       PIC X(1).
                   88  DE-SEND-ERROR-TO-TERM VALUE 'Y' SPACES.
                   88  DE-DONT-SEND-ERROR-TO-TERM
                                             VALUE 'N'.
               05  DE-ERROR-OPTION           PIC X(1).
                   88  DE-RETURN-ERROR-MSG-TO-CALLER
                                            VALUE 'R' SPACES.
                   88  DE-HANDLE-ERROR-IN-SUBROUTINE
                                            VALUE 'S'.
110499         05  DE-TRANS-RID              PIC X(12).
110499         05  DE-TRANS-ORIGIN           PIC X(5).
               05  DE-ERROR-RETURN-SUB       PIC 9(3).
062002         05  DE-ORIGINATOR             PIC X(8).
050604         05  DE-REQUEST-SOURCE-IND     PIC X(1).
050604             88  DE-BETALINK-REQ      VALUE 'A'.
050604             88  DE-BLSERVER-REQ      VALUE 'B'.
050604             88  DE-THIN-CLIENT-REQ   VALUE 'C'.
050604             88  DE-PUBLIC-REQ        VALUE 'D'.
081410             88  DE-CONVERSION-REQ    VALUE 'V'.
060304         05  DE-MAX-BUFFER-IND         PIC X.
081410         05  DE-FILE-OR-DB-MODE        PIC X.
081410             88  DE-FILE-MODE         VALUE 'F'.
081410             88  DE-DB-MODE           VALUE 'D' ' '.
040816         05  DE-USER-ID-TYPE           PIC X(2).
010617         05  DE-INTERNAL-REGION-SW     PIC X(1).
010617             88  DE-BETA-INTERNAL      VALUE 'Y'.
010617             88  DE-EXTERNAL-COMPANY   VALUE 'N'.
101318         05  DE-RECORD-WAS-RETURNED-IND PIC X(1).
101318         05  FILLER                    PIC X(60).
               05  DE-LAST-SET-ID.
                   10  DE-LAST-SET-RID   PIC X(22).
                   10  DE-LAST-SET-URI   PIC X(11).
                   10  DE-LAST-SET-FILL  PIC X(12).
               05  DE-KEY-AREA.
                   10  DE-SEARCH-FORMAT  PIC X.
                   10  DE-KEY            PIC X(40).
 ******  ACCT LAYOUT  - DEACT2
           04  DE-ACCT-KEY-AREA             PIC X(50).
           04  FILLER  REDEFINES DE-ACCT-KEY-AREA.
               05  ACCT-KEY-ACCT-NO         PIC 9(8).
           04  DE-ACCT-LAYOUT.
               05  ACCT-ACCT-NO             PIC 9(8).
               05  ACCT-REP                 PIC X(4).
               05  ACCT-CONV-ACCT           PIC X(12).
               05  ACCT-ZIP-CODE            PIC 9(5).
               05  ACCT-ZIP-REST            PIC 9(4).
               05  ACCT-FOREIGN-ZIP         PIC X(10).
               05  ACCT-NAMELINES           PIC 9(1).
               05  ACCT-BROKR               PIC 9(5).
               05  ACCT-NALINES             PIC X(180).
               05  FILLER REDEFINES ACCT-NALINES.
                   07  ACCT-LINE-ONE        PIC X(30).
                   07  ACCT-LINE-TWO        PIC X(30).
                   07  ACCT-LINE-THREE      PIC X(30).
                   07  ACCT-LINE-FOUR       PIC X(30).
                   07  ACCT-LINE-FIVE       PIC X(30).
                   07  ACCT-LINE-SIX        PIC X(30).
               05  FILLER REDEFINES ACCT-NALINES.
                   07  ACCT-LINE-X OCCURS 6 TIMES
                                            PIC X(30).
               05  ACCT-FIRST-NAME          PIC X(30).
               05  ACCT-LAST-NAME           PIC X(30).
               05  ACCT-PHONE               PIC 9(12).
               05  ACCT-PHONE2              PIC 9(12).
               05  ACCT-PHONE3              PIC 9(12).
               05  ACCT-STATE-CODE          PIC X(2).
               05  ACCT-CNTRY               PIC X(2).
               05  ACCT-RES-ID              PIC X(2).
               05  ACCT-PROV                PIC X(2).
               05  ACCT-CITY                PIC X(16).
               05  ACCT-NO-AUTO-CXL         PIC X(1).
               05  ACCT-W8-MAILING-YY       PIC 9(2).
               05  ACCT-NOID                PIC X(1).
               05  ACCT-IDFORMAT            PIC X(1).
060701         05  ACCT-ID-FILL             PIC X(2).
060701         05  ACCT-ID                  PIC 9(9).
               05  ACCT-OPEN-CYMD           PIC 9(8).
               05  ACCT-CHANGE-CYMD         PIC 9(8).
               05  ACCT-CASHSI              PIC X(2).
               05  ACCT-MARGSI              PIC X(2).
               05  ACCT-FORGNSI             PIC X(2).
               05  ACCT-DIVSI               PIC X(1).
               05  ACCT-SECOND-CSI          PIC X(2).
               05  ACCT-W9-REQUEST          PIC X(1).
               05  ACCT-W8-REQUEST          PIC X(1).
               05  ACCT-TEFRA               PIC X(1).
               05  ACCT-PRINCIPAL           PIC X(1).
               05  ACCT-INST                PIC X(1).
               05  ACCT-NYTAX               PIC X(1).
               05  ACCT-INVOBJ              PIC X(1).
               05  ACCT-CLASS               PIC X(1).
               05  ACCT-FORMS               PIC 9(11).
               05  ACCT-MISC6               PIC X(1).
               05  ACCT-DIR-SW              PIC X(1).
               05  ACCT-DISCL-CODE          PIC X(1).
               05  ACCT-DIV-RND             PIC X(1).
               05  ACCT-DISCR               PIC X(1).
               05  ACCT-OPTN-I-O            PIC X(1).
               05  ACCT-AVG-PRICE           PIC X(1).
               05  ACCT-DLR-SW1             PIC X(1).
               05  ACCT-DLR-SW2             PIC X(1).
               05  ACCT-DLR-SW3             PIC X(1).
               05  ACCT-DLR-SW4             PIC X(1).
               05  ACCT-DLR-SW5             PIC X(1).
               05  ACCT-DLR-SW6             PIC X(1).
040705         05  ACCT-HOME-PHONE-2        PIC X(12).
040705         05  FILLER                   PIC X(3).
               05  ACCT-OUT-MMF             PIC X(1).
               05  ACCT-NO-BILL             PIC X(1).
               05  ACCT-INTR                PIC X(1).
               05  ACCT-MARG-INT-CHG        PIC X(1).
               05  ACCT-SEGREGATION         PIC X(1).
               05  ACCT-CRINT               PIC 9(1).
               05  ACCT-CRINT-NOPOST        PIC X(1).
               05  ACCT-OPTN-MRG-LMT        PIC 9(7).
               05  ACCT-ERISA-CODE          PIC X(1).
               05  ACCT-FLIP-EXCH           PIC 9(2).
               05  ACCT-NO-CFM              PIC X(1).
               05  ACCT-COMM-SCHED-STK      PIC 9(2).
               05  ACCT-COMM-SCHED-BND      PIC 9(2).
               05  ACCT-COMM-SCHED-OPT      PIC 9(2).
               05  ACCT-STKDISCNT-FILL      PIC X(1).
               05  ACCT-STKDISCNT           PIC 9V99.
               05  ACCT-BNDDISCNT-FILL      PIC X(1).
               05  ACCT-BNDDISCNT           PIC 9V99.
               05  ACCT-OPTDISCNT-FILL      PIC X(1).
               05  ACCT-OPTDISCNT           PIC 9V99.
               05  ACCT-CFM                 PIC 9(1).
               05  ACCT-STAT                PIC 9(1).
               05  ACCT-ACAT-TERM-FEE-SW    PIC X(1).
               05  ACCT-MANO                PIC 9(3).
               05  ACCT-RESTRICT-IND        PIC X(1).
               05  ACCT-RESTRICT-CYMD       PIC 9(8).
               05  ACCT-MST-BRKR            PIC X(1).
               05  ACCT-COMPANY-MERGE-CDE   PIC X(2).
               05  ACCT-NO-JOURNAL-IND      PIC X(1).
               05  ACCT-MMFSVCFEE           PIC X(1).
               05  ACCT-USE-ZIP4            PIC X(1).
               05  ACCT-DOWNLOAD-SW         PIC X(1).
               05  ACCT-INST-OVERRIDE       PIC X(1).
               05  ACCT-POSTAGE-AMT-FILL    PIC X(1).
               05  ACCT-POSTAGE-AMT         PIC 9(3)V9(2).
               05  ACCT-NON-CUST            PIC X(1).
               05  ACCT-PURGE-IND           PIC X(1).
               05  ACCT-IRS-NALINE1         PIC 9(1).
               05  ACCT-IRS-NALINE2         PIC 9(1).
               05  ACCT-REP-B4-ACAT         PIC X(4).
               05  ACCT-REMIC-CMO-SW        PIC X(1).
               05  ACCT-ACCT-CAT            PIC 9(3).
               05  ACCT-TRAN-LVL            PIC 9(3).
               05  ACCT-PRIME-BRKR          PIC X(1).
               05  ACCT-ACCT-TYPE-IND       PIC X(1).
               05  ACCT-PHONE-FAX           PIC 9(12).
               05  ACCT-REST-BAL-FILL       PIC X(1).
               05  ACCT-REST-BAL            PIC S9(7).
               05  ACCT-DELIVERY-PT-NO      PIC 9(2).
               05  ACCT-DELIVERY-PT-CHK     PIC 9(1).
               05  ACCT-STMT-CODE           PIC X(1).
               05  ACCT-NETWORK-ELIG        PIC X(1).
051113         05  FILLER                   PIC X(1).
               05  ACCT-MMF-SWEEP           PIC X(1).
               05  ACCT-CASH-DIV            PIC X(1).
               05  ACCT-CASH-ONLY           PIC X(1).
051113         05  FILLER                   PIC X(1).
               05  ACCT-MMF-FUNDS.
                   06  ACCT-MMF-FUND-1      PIC X(1).
                   06  ACCT-MMF-FUND-2      PIC X(1).
                   06  ACCT-MMF-FUND-3      PIC X(1).
                   06  ACCT-MMF-FUND-4      PIC X(1).
               05  FILLER REDEFINES ACCT-MMF-FUNDS.
                   06  ACCT-MMF-FUND OCCURS 4 TIMES
                                            PIC X(1).
               05  ACCT-MGT-GROUP           PIC X(4).
               05  ACCT-MAIL-DIVERT-CD      PIC X(4).
               05  ACCT-PREVENT-PRIN-TRADES PIC X(1).
               05  ACCT-TPA-NUMBER          PIC X(4).
               05  ACCT-BOLT-IND            PIC X(1).
               05  ACCT-INVESTOR-CODE       PIC X(1).
               05  ACCT-DSC-SHR-EXEMPT-SW   PIC X(1).
               05  ACCT-PRODUCT-CLASS       PIC X(4).
062600         05  ACCT-NO-REBATE-IND       PIC X(1).
110200         05  ACCT-NAV                 PIC X(1).
110200         05  ACCT-NAVR-SW             PIC X(1).
               05  ACCT-NCHG-SEC-FEE        PIC X(1).
               05  ACCT-CMTA-NO             PIC 9(5).
               05  ACCT-LRATE-FILL          PIC X(1).
               05  ACCT-LRATE               PIC 9(2)V9(4).
               05  ACCT-BRATE-FILL          PIC X(1).
               05  ACCT-BRATE               PIC 9(2)V9(4).
               05  ACCT-NCCNO               PIC 9(5).
               05  ACCT-FBO                 PIC X(1).
               05  ACCT-NASDAQ              PIC X(5).
               05  ACCT-OTHER-DEP           PIC X(6).
               05  ACCT-GENERIC-USER-FLD    PIC 9(5).
               05  ACCT-MUNI-CNS-PART       PIC X(1).
               05  ACCT-CUST-NASD-SYM       PIC X(5).
               05  ACCT-ORDR-ONLY           PIC X(1).
               05  ACCT-REINVEST            PIC X(1).
               05  ACCT-CASH-DR-OVRD        PIC X(1).
               05  ACCT-F121-SW             PIC X(1).
               05  ACCT-GSCC                PIC 9(5).
               05  ACCT-MBSCC               PIC X(5).
               05  ACCT-EMP-CLASS-CODE      PIC X(5).
               05  ACCT-LST-REVIEW-CODE     PIC X(1).
               05  ACCT-LST-REVIEW-CYMD     PIC 9(8).
               05  ACCT-DOC-RESTRICT-CODE   PIC X(1).
               05  ACCT-DOC-RESTRICT-CYMD   PIC 9(8).
               05  ACCT-GROUPNO             PIC 9(9).
051113         05  FILLER                   PIC X.
030304         05  ACCT-IPO-ELIGIBLE        PIC X(1).
022804         05  ACCT-AVG-PRICE-IND       PIC X(1).
060304         05  ACCT-ST-REG-EXEMPT       PIC X(1).
101603         05  ACCT-STAT-OLD            PIC X(1).
101603         05  ACCT-MMF-SUPPRESS-OLD    PIC X(1).
               05  ACCT-MMF-SUPPRESS        PIC X(1).
100401         05  ACCT-ALPHA-KEYS.
100401             06  ACCT-ALPHA-KEY1      PIC X(20).
100401             06  ACCT-ALPHA-KEY2      PIC X(20).
100401             06  ACCT-ALPHA-KEY3      PIC X(20).
100401             06  ACCT-ALPHA-KEY4      PIC X(20).
100401             06  ACCT-ALPHA-KEY5      PIC X(20).
100401         05  FILLER REDEFINES ACCT-ALPHA-KEYS.
100401             06  ACCT-ALPHA-KEY-TBL OCCURS 5 TIMES PIC X(20).
               05  ACCT-MMF-INIT-PURCH      PIC X(1).
               05  ACCT-DOCA-STATUS-CODE    PIC X(1).
               05  ACCT-IRAS-IRA-TYPE       PIC X(5).
               05  ACCT-IRAS-BIRTH-CYMD     PIC 9(8).
               05  ACCT-IRAS-XREF-ACCT      PIC 9(8).
               05  ACCT-IRAS-FEE-SCHEDULE   PIC 9(3).
               05  ACCT-IRAS-FEE-STATUS     PIC X(1).
               05  ACCT-IRAS-PRI-SEC-CODE   PIC X(1).
               05  ACCT-IRAS-FEE-BYPASS-SW  PIC X(1).
               05  ACCT-ACH-INC             PIC X(1).
               05  ACCT-ACH-CR              PIC X(1).
               05  ACCT-ACH-DR              PIC X(1).
081601         05  ACCT-MMF-FUND-SYM        PIC X(3).
081601         05  ACCT-MMF-SELL-FIRST-SYM  PIC X(3).
112001         05  ACCT-BYPASS-REJECT-ACAT  PIC X(1).
112801         05  ACCT-BRS-BRANCH          PIC X(4).
112801         05  ACCT-B4-ACAT-BRS-BRANCH  PIC X(4).
100202         05  ACCT-TITLE-TEXT-LINES    PIC X(120).
100202         05  FILLER REDEFINES ACCT-TITLE-TEXT-LINES.
100202             06  ACCT-TITLE-TEXT1     PIC X(30).
100202             06  ACCT-TITLE-TEXT2     PIC X(30).
100202             06  ACCT-TITLE-TEXT3     PIC X(30).
100202             06  ACCT-TITLE-TEXT4     PIC X(30).
100202         05  FILLER REDEFINES ACCT-TITLE-TEXT-LINES.
100202             06  ACCT-TITLE-TEXT      OCCURS 4 TIMES
100202                                      PIC X(30).
100202         05  ACCT-ACT-TTL-EXP-DATE-IND
100202                                      PIC X(1).
100202         05  ACCT-ACT-TTL-EXP-DATE    PIC X(10).
100202         05  ACCT-ACT-TTL-EFF-DATE-IND
100202                                      PIC X(1).
100202         05  ACCT-ACT-TTL-EFF-DATE    PIC X(10).
100202         05  ACCT-CUSTOMER-ID         PIC 9(18).
030603         05  ACCT-BRR-ELIGIBLE-SW     PIC X(1).
041703         05  ACCT-GENDER              PIC X(1).
041703         05  ACCT-HOUSEHOLD-NET-INCOME
041703                                      PIC X(1).
041703         05  ACCT-NET-WORTH           PIC X(1).
041703         05  ACCT-LIQUID-NET-WORTH    PIC X(1).
092003         05  ACCT-ACCT-STATUS-IND     PIC X(1).
092003         05  ACCT-OLD-ACCT-STATUS-IND PIC X(1).
040704         05  ACCT-ACQUIRED            PIC X(1).
040705         05  ACCT-CELL-PHONE-1        PIC X(12).
040705         05  ACCT-CELL-PHONE-2        PIC X(12).
040705         05  ACCT-BUS-PHONE-2         PIC X(12).
111216         05  ACCT-FIDUCIARY           PIC X(1).
111216         05  ACCT-EXEMPTION           PIC X(1).
111117         05  ACCT-TBA-EXCLUDED        PIC X(1).
111117         05  ACCT-TBA-EXEMPT          PIC X(1).
111705         05  ACCT-REMAIN-FREE-TRDS    PIC 9(3).
042006         05  ACCT-ALRT-ACRONYM        PIC X(8).
042006         05  ACCT-ALRT-ACCESS-CODE    PIC X(16).
072006         05  ACCT-AED-IND             PIC X(1).
092106         05  ACCT-STK-LOAN-TRANS-IND  PIC X(1).
092106         05  ACCT-SOCIAL-CODE         PIC X(2).
071611         05  ACCT-LST-STMT-MMYY       PIC 9(4).
071611         05  ACCT-COMM-OVERRIDE-SW    PIC X(1).
081311         05  ACCT-INST-ACCT-IND       PIC X(1).
041813         05  ACCT-INV-OPTION-CODE     PIC X(1).
112115         05  ACCT-NATURAL-PERSON      PIC X(1).
091716         05  ACCT-COMPANY-MERGE-CODE  PIC X(2).
111A17         05  ACCT-MULTI-CNY-IND       PIC X(1).
111A17         05  ACCT-ACT-CMP-RVW-IND     PIC X(1).
111A17         05  FILLER                   PIC X(2056).
041710         05  ACCT-OLD-NAKEYS          PIC X(100).
041809***** THE FILLER REDUCED BY 9 FOR THE FOLLOWING THREE FIELD
041809***** TO CONTROL CRNCY CHANGE AND PAD RECORD REQUIREMENTS
041809         05  ACCT-CRNCY-CHANGE-CODE-X PIC X(3).
041809         05  ACCT-CRNCY-CHANGE-CODE   PIC X(3).
041809         05  ACCT-CRNCY-CURRENT-SEQ   PIC S9(5) COMP-3.
      *****  THE FOLLOWING ARE FIELDS PASSED BETWEEN THE DIFFERENT
      *****  ENGINES AND ARE NOT USED AS INPUT.
061408         05  ACCT-CHG-UPDT            PIC X(1).
041208         05  ACCT-OLD-PPT             PIC X(1).
100506         05  ACCT-OLD-REINVEST-CODE   PIC X(1).
042105         05  ACCT-FIRM-NO             PIC 9(3).
042105         05  ACCT-SUB-NO              PIC 9(3).
100704         05  ACCT-OLD-PRINCIPAL       PIC X(1).
022704         05  ACCT-OLD-PRODUCT-CLASS   PIC X(4).
030404         05  ACCT-OLD-NAV             PIC X(1).
040704         05  ACCT-OLD-DIVSI           PIC X.
100202         05  ACCT-USE-CUST-ID-SW      PIC X(1).
112801         05  ACCT-OLD-BRS-BRANCH      PIC X(4).
090601         05  ACCT-WITHHOLD-RYMD       PIC 9(7).
051113         05  FILLER                   PIC X(1).
081601         05  ACCT-MMF-SEC-NO          PIC 9(9).
081601         05  ACCT-OLD-SEC-NO          PIC 9(9).
081601         05  ACCT-MMF-SELL-FIRST-SEC-NO
081601                                      PIC 9(9).
               05  ACCT-OLD-NALINES         PIC X(180).
               05  ACCT-OLD-ZIP             PIC 9(5).
               05  ACCT-OLD-ZIP-REST        PIC 9(4).
               05  ACCT-OLD-REP             PIC X(4).
               05  ACCT-DELAWARE-SW         PIC X(1).
