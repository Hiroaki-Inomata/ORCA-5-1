      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ************************************************************
      *    �쥻�ץ����ٽ�     COPY (CPRCF064.INC)                *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-����    03.09.24  �������Խ��ե饰�ɲ�
      *  01.00.02    NACL-����    04.02.05  ���ť����ɡ����̤��߷�
      *                                     �ɲ�
      *  ��������ǯ��������б�
      *  01.01.01    NACL-����    04.04.05  ������Ʃ���Ѳ��Ƴ������
      *                                     ���ɲ�
      *  01.01.02    NACL-����    05.01.17  ���Ǥ��ɲ�
      *  01.01.03    NACL-����    05.02.28  �ʥ��˥ե饰���ɲ�
      *  02.09.01    NACL-����    06.04.17  ��������ǯ��������б�
      *  03.00.01    NACL-����    06.06.22  ���ǡ����̱��Ǥ��ɲ�
      *  03.01.01    NACL-����    06.07.11  �߻ٱ�˴ؤ�����Ť��ɲ�
      *  03.03.01    NACL-ƣ��    06/09/29  ������Ƚ�����ε�����ˡ���ѹ�
      *                                     �������»�Ƚ�Ǥ��ɲ�
      *  03.05.01    NACL-��Ʀ��  07/05/30  ���롼�׿����б�
      *  03.05.02    NACL-����    08/04/10  �쥻�����������û��ե饰�ɲ�
      *  04.03.01    NACL-����    08/07/14  �����������ե饰���ɲ�
      *
      *  04.04.01    NACL-����    09/02/04  �����ʣ£Σ����ˤ��ɲ�
      *  04.04.02    NACL-����    10/03/29  �������ǡʣ����ܰʹ߸��������
      *                                     ���ɲ�
      *  04.04.03    NACL-����    10/04/06  �����б������ɲ�
      *
      *  04.05.01    NACL-ƣ��    09/06/10  Ŧ�������ؤΥ����ȵ����б�
      *                                     ���ż��̶�ʬ���ɲ�
      *  04.05.02    NACL-ƣ��    09/07/14  �������ʲ������б�
      *  04.05.03    NACL-����    09/11/30  �쥻�ťե����ޥåȥ����ȳ�ĥ
      *                                     �б��ʥե������Խ���
      *  04.05.04    NACL-����    10/01/12  �޶�ʬ�ɲ�
      *  04.05.05    NACL-����    11/11/30  �������ǥե�����ɲ�
      *  04.05.06    NACL-����    12/04/10  �����б������ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *  04.06.02    NACL-ƣ��    11/01/05  ��ǽ�ɲá����û���Ʊ��ʻ������
      *
      *  04.07.01    NACL-����    12/09/25  �����Ⱥ��������ɲ�
      *  04.07.02    NACL-ƣ��    12/12/17  ʣ����쥻�ץ�ʬ���б�
      *  04.07.03    NACL-����    14/04/07  �����б������ɲ�
      *  04.07.04    NACL-����    15/08/10  �����ݸ��ȹ礻�ֹ��ɲ�
      *
      *  04.08.01    NACL-����    13/01/25  ������ʬ�������ѥ쥻�ץȵ����б�
      *  04.08.02    NACL-����    16/03/31  ʿ������ǯ��������б�
      *  04.08.03    NACL-����    16/08/02  �����߰ʲ����ޤ�����ѥ쥻�ץȵ���
      *                                     �ʵ�Ͽ���б�
      *  04.08.04    NACL-����    16/08/23  �������б�
      *  04.08.05    NACL-����    18/04/17  ʿ������ǯ��������б�
      *  04.08.06    NACL-����    18/09/27  ���򼰥������б�
      *
      *  05.00.01    NACL-����    19/05/16  �ౡ����Ʊ��Ƴ���������ȼ�ư����
      *                                     �б�
      *  05.00.02    NACL-����    20/04/07  ���£�ǯ��������б�
      ************************************************************
      *01  RECE64-REC.
           03  RECE64-KEY.
               05  RECE64-KEY1.
      *���ŵ��أɣ�
                   07  RECE64-HOSPNUM      PIC 9(2).
      *����ǯ��
                   07  RECE64-SRYYM        PIC 9(06).
      *������ʬ
                   07  RECE64-NYUGAIKBN    PIC X(01).
      *���ԣɣ�
                   07  RECE64-PTID         PIC 9(10).
      *�쥻�ץȲ�
                   07  RECE64-RECEKA       PIC X(02).
      *������ʧ����
                   07  RECE64-TEISYUTUSAKI
                                           PIC 9(01).
      *�쥻�ץȼ���
                   07  RECE64-RECESYUBETU  PIC 9(04).
      *�ݸ����ֹ�        
                   07  RECE64-HKNJANUM     PIC X(08).
      *�����ʬ
                   07  RECE64-HOJOKBN      PIC X(01).
      *����ɣ�
                   07  RECE64-KOHID        PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE64-TEKSTYMD     PIC X(08). 
      *ʬ���ʬ
                   07  RECE64-RECEIPT-KBN  PIC X(10).
      *���ֹ�
               05  RECE64-ZAINUM          PIC  9(08).
      *��Ϣ��
               05  RECE64-ZAIREN          PIC  9(02).
      *����ʬΥ��ʬ
           03  RECE64-KOHBUNRI            PIC  9(01).
      *���Ŷ�ʬ
           03  RECE64-SRYKBN              PIC  X(02).
      *���ż��̶�ʬ
           03  RECE64-SRYSYUKBN           PIC  X(03).
      *�޼��̶�ʬ
           03  RECE64-ZAISKBKBN           PIC  X(01).
      *�����ֹ�
           03  RECE64-PTNUM               PIC  X(20).
      *���Ų�
           03  RECE64-SRYKA               PIC  X(02).
      *�ݸ��ȹ礻�ֹ�
           03  RECE64-HKNCOMBI            PIC  9(04).
      *���ݶ�ʬ
           03  RECE64-JIHOKBN             PIC  X(01).
      *������
           03  RECE64-ZAITEN              PIC  9(08).
      *���ť����ɷ�
           03  RECE64-SRYCDTOTAL          PIC  9(14).
      *���̷�
           03  RECE64-SURYOUTOTAL         PIC  9(07)V9(05).
      *���ť����ɡ����̤��߷�
           03  RECE64-RUIKEITOTAL         PIC  9(13)V9(05).
      *���ٿ�
           03  RECE64-MEISAISU            PIC  9(04).
      *�޲��
           03  RECE64-ZAIKAISU            PIC  9(04).
      *�굻������
           03  RECE64-SYUTEN1             PIC  9(07).
      *�굻������
           03  RECE64-SYUTEN2             PIC  9(07).
      *��������
           03  RECE64-YKZTEN              PIC  9(07).
      *�������
           03  RECE64-KIZAITEN            PIC  9(07).
      *�ɥ�����������
           03  RECE64-DRCD                PIC  X(05).
      *������ե饰
           03  RECE64-ZAIREQFLG           PIC  X(01).
      *����Ψ
           03  RECE64-TEIGENRATE          PIC  X(01).
      *�����ȥ����ɥե饰
           03  RECE64-COMCDFLG            PIC  X(01).
      *�ʥ��˥ե饰
           03  RECE64-SAFLG               PIC  X(01).
      *�����������ʣ����ϣ��ʥ����ȤΤߡˡ�
           03  RECE64-TOYAKUFLG           PIC  X(01).
      *�������������������������ٿ�
           03  RECE64-RYOTENKISAISU       PIC  9(04).
      *�ط�������¸�ߥե饰
           03  RECE64-SETCOMFLG           PIC  X(01).
      *���򼰥�����¸�ߥե饰
           03  RECE64-SENTAKUCOMFLG       PIC  X(01).
      *�޶�ʬ
           03  RECE64-ZAIKBN              PIC  9(03).
      *�������ǡʣ����ܰʹ߸��������
           03  RECE64-GAZOGEN             PIC  X(01).
      *�ƿǲû��������ˤκ�
           03  RECE64-NYUSAIFLG           PIC  X(01).
      *�����߰ʲ����ޥ쥻�ץȵ��ܥե饰
           03  RECE64-YKZKISAIFLG         PIC  X(01).
      *��������ʬ
           03  RECE64-JISANYAKKBN         PIC  X(01).
      *
      *��ϥӥ�
           03  RECE64-RIHABIRI-COMMENT    PIC  X(01).
      *��ϥӥ�ʿ����ɼ�����
           03  RECE64-SINDAI-COMMENT      PIC  X(01).
      *��ϥӥ�ʸƵ۴��
           03  RECE64-KOKYUKI-COMMENT     PIC  X(01).
      *��ϥӥ��Ǿ��ɼ�������
           03  RECE64-NOUKE-COMMENT       PIC  X(01).
      *��ϥӥ�ʱ�ư���
           03  RECE64-UNDOKI-COMMENT      PIC  X(01).
      *��ϥӥ�����´��ԡ�
           03  RECE64-NANBYO-COMMENT      PIC  X(01).
      *��ϥӥ�ʾ㳲���ʼԡˡ�
           03  RECE64-SYOGAI-COMMENT      PIC  X(01).
      *��ϥӥ���ݿ���
           03  RECE64-SESSYOKU-COMMENT    PIC  X(01).
      *��ϥӥ�����Ѿɸ�����
           03  RECE64-HAIYO-COMMENT       PIC  X(01).
      *��ã����
           03  RECE64-KAITATU-COMMENT     PIC  X(01).
      *
      *��ϥӥ�û��ʿ����ɼ�����
           03  RECE64-SINDAI9-COMMENT     PIC  X(01).
      *��ϥӥ�û��ʸƵ۴��
           03  RECE64-KOKYUKI9-COMMENT    PIC  X(01).
      *��ϥӥ�û������Ѿɸ�����
           03  RECE64-HAIYO9-COMMENT      PIC  X(01).
      *
      *���޴�����Ƴ��
           03  RECE64-FLG-YAKUKAN-COMMENT PIC  X(01).
      *�������ܿ�����Ƴ��
           03  RECE64-FLG-NYUEIYO-COMMENT PIC  X(01).
      *�ౡ��ˬ���Ƴ��
           03  RECE64-FLG-HOUMON-COMMENT  PIC  X(01).
      *���¼�����ո��������
           03  RECE64-FLG-SYOBYO-COMMENT  PIC  X(01).
      *������Ʊ�ս������
           03  RECE64-FLG-RYOYO-COMMENT   PIC  X(01).
      *�ౡ��������Ƴ�û�����������޴�����Ƴ��
           03  RECE64-FLG-TAIIN-COMMENT   PIC  X(01).
      *ˬ��Ǹ�ؼ���
           03  RECE64-FLG-HOUSIJI-COMMENT PIC  X(01).
      *��ѡ�͢��Ͻ�����
           03  RECE64-FLG-SHUJUTU-COMMENT PIC  X(01).
      *�ãԡ��ͣң�
           03  RECE64-FLG-CTMRI-COMMENT   PIC  X(01).
      *�������ౡ��ˬ���Ƴ��
           03  RECE64-FLG-HOUSEI-COMMENT  PIC  X(01).
      *�����ʳ�Υ�¼������û����������û���
           03  RECE64-FLG-KAKURI-COMMENT  PIC  X(01).
      *������Ʃ���Ѳ��Ƴ������
           03  RECE64-FLG-HINKAI-COMMENT  PIC  X(01).
      *����
           03  RECE64-FLG-SANSO-COMMENT   PIC  X(01).
      *��ǡ�Ʊ��ʣ�����Ųʡ�
           03  RECE64-FLG-SYODOU-COMMENT  PIC  X(01).
           03  RECE64-SYODOU-SRYKA-AREA.
               05  RECE64-SYODOU-SRYKA    PIC  X(02)     OCCURS  10.
      *�������»�Ƚ��
           03  RECE64-KNSJISKBN           PIC  9(01).
      *�������»�Ƚ�ǥ��롼��
           03  RECE64-KNSJISGRPKBN        PIC  9(02).
      *���ǡ����̱���
           03  RECE64-FLG-OSIN-COMMENT    PIC  X(01).
      *�߻ٱ�˴ؤ������
           03  RECE64-FLG-ZAISIEN-COMMENT PIC  X(01).
      *�쥻�����������û��ե饰
           03  RECE64-NYUKSNFLG           PIC  9(01).
      *�����ʣ£ΣС��Σԡݣ���£ΣС��ȣ��ΣС�
           03  RECE64-FLG-KENSA-COMMENT   PIC  X(01).
      *
      *���ٱ�Ϣ�Ȼ�Ƴ��
           03  RECE64-FLG-KAISIEN-COMMENT PIC  X(01).
      *���ı��ܿ�����Ƴ��
           03  RECE64-FLG-SYUEIYO-COMMENT PIC  X(01).
      *�͹���¡��
           03  RECE64-FLG-JINZOU-COMMENT  PIC  X(01).
      *�ɽ걢���ĺ�����
           03  RECE64-FLG-KYOKUIN-COMMENT PIC  X(01).
      *���
           03  RECE64-FLG-MASUI-COMMENT   PIC  X(01).
      *���ܥ��ݡ��ȥ�����û�
           03  RECE64-FLG-EISAPO-COMMENT  PIC  X(01).
      *�Ƶۥ���������û�
           03  RECE64-FLG-KOKKEA-COMMENT  PIC  X(01).
      *�žɼԲû�����������������������
           03  RECE64-FLG-JYUSYO-COMMENT  PIC  X(01).
      *
      *�������ǥե����
           03  RECE64-FLG-FILM-COMMENT    PIC  X(01).
      *
      *������ӽ��۰����ؼ���
           03  RECE64-FLG-KAISYOKU-COMMENT
                                          PIC  X(01).
      *�����ʥ��硼�ȡ������������ʥǥ�������
           03  RECE64-FLG-SEIKEA-COMMENT  PIC  X(01).
      *�����ʥꥨ���������û�
           03  RECE64-FLG-SEIRIE-COMMENT  PIC  X(01).
      *�������޶�̳�»ܲû�
           03  RECE64-FLG-BYOYAK-COMMENT  PIC  X(01).
      *
      *�����³�۰��Ƶ���ˡ��Ƴ������
           03  RECE64-FLG-YOUATU-COMMENT  PIC  X(01).
      *
      *��¡�ڡ����᡼������Ƴ�����������ѷ���ư����ư���
      *��¡�ڡ����᡼������Ƴ�������ʥ�������������ư��ܹԴ��û��ޤ��
           03  RECE64-FLG-SINPESUR-COMMENT  PIC  X(01).
      *��֥�˥���󥰲û�
           03  RECE64-FLG-SINPESU-COMMENT   PIC  X(01).
      *
      *���󴵼Ի�Ƴ������������
           03  RECE64-FLG-GANSIDO-COMMENT   PIC  X(01).
      *���𴵼Զ�Ʊ������
           03  RECE64-FLG-ZAIKYO-COMMENT    PIC  X(01).
      *�����ˤ��������ɷ�ˡ
           03  RECE64-FLG-BOKOHO-COMMENT    PIC  X(01).
      *Ķ�žɻ��ʼԡ��������Ųû�����Ķ�žɻ��ʼԡ��������Ųû�
           03  RECE64-FLG-CYOJKSN-COMMENT   PIC  X(01).
      *��Ǣ��Ω��Ƴ��
           03  RECE64-FLG-HAIJRT-COMMENT    PIC  X(01).
      *����Ⱦ�Ƿ����ܷдɱ���ˡ��Ƴ������
           03  RECE64-FLG-ZAIHANKO-COMMENT  PIC  X(01).
      *�ⵤ�����Ǽ���
           03  RECE64-FLG-KIATU-COMMENT     PIC  X(01).
      *������������ץ������û�
           03  RECE64-FLG-SCPRO-COMMENT     PIC  X(01).
      *
      *���ֳ��۵ޱ��⸡���û��������®���θ����û�
           03  RECE64-FLG-KENKSN-COMMENT    PIC  X(01).
      *���ֳ��۵ޱ���������ǲû�
           03  RECE64-FLG-KINGAZ-COMMENT    PIC  X(01).
      *
      *�ౡ����Ʊ��Ƴ����
           03  RECE64-FLG-TAIKYO-COMMENT    PIC  X(01).
      *
      *�͹��⼪����Ĵ���û�
           03  RECE64-FLG-JINNAI-COMMENT    PIC  X(01).
      *
      *��Ǣ��Ω�ٱ�û�
           03  RECE64-FLG-HAIJSK-COMMENT    PIC  X(01).
      *
      *��������ơ��֥�
           03  RECE64-DAY-AREA.
      *        ���Ų���ơ��֥��1:���,2:����,3:����, 4:����ʾ��
             04  RECE64-DAY-TBL           OCCURS  4.
                 05  RECE64-KAISUINF      OCCURS  31.
                     07  RECE64-KAISU     PIC  9(03).
      *
      *�굻������ơʼ�ѡ��������ǡ����֡�
           03  RECE64-ACCTP-SYUGI-TBL-G.
               05  RECE64-ACCTP-SYUGI-TBL  OCCURS  5.
      *�굻���ϥ�����
                   07  RECE64-ACCTP-SRYCD  PIC X(09).
      *�굻����
                   07  RECE64-ACCTP-SYUTEN PIC 9(07).
      *��������
                   07  RECE64-ACCTP-YKZTEN PIC 9(07).
      *����¾��������ʥե���ࡦ���ǡ����ǰʳ���
                   07  RECE64-ACCTP-KIZAITEN
                                           PIC 9(07).
      *�ե��������
                   07  RECE64-ACCTP-FIRTEN PIC 9(07).
      *��������
           03  RECE64-ACCTP-SANSOTEN       PIC 9(07).
      *��������
           03  RECE64-ACCTP-CHISOTEN       PIC 9(07).
      *
      *������ʬ��������
           03  RECE64-HOKATU-TENSU-AREA.
      *������
               05  RECE64-HOKATU-ZAITEN    PIC 9(08).
      *�굻������
               05  RECE64-HOKATU-SYUTEN1   PIC 9(07).
      *�굻������
               05  RECE64-HOKATU-SYUTEN2   PIC 9(07).
      *��������
               05  RECE64-HOKATU-YKZTEN    PIC 9(07).
      *�������
               05  RECE64-HOKATU-KIZAITEN  PIC 9(07).
      *
      *�����ݸ��ȹ礻�ֹ�
           03  RECE64-HKNCOMBI-DAY-AREA.
               05  RECE64-HKNCOMBI-DAY     PIC 9(04)     OCCURS  31.