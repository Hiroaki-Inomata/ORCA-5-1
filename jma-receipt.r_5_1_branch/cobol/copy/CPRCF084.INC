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
      *    ϫ�ҥ쥻�ץ����ٽ��������     COPY (CPRCF084.INC)    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-����    04.02.05  ���ť����ɡ����̤��߷�
      *                                     �ɲ�
      *  ��������ǯ��������б�
      *  01.01.01    NACL-����    04.04.05  ������Ʃ���Ѳ��Ƴ������
      *                                     ���ɲ�
      *  01.01.02    NACL-����    05.01.17  ���Ǥ��ɲ�
      *  01.01.03    NACL-����    05.02.28  �ʥ��˥ե饰���ɲ�
      *  02.09.01    NACL-����    06.04.17  ��������ǯ��������б�
      *  03.00.01    NACL-����    06.06.22  ���ǡ����̱��Ǥ��ɲ�
      *  03.01.01    NACL-����    06.07.11  �߻ٱ�˴ؤ�����Ť��ɲ�
      *  03.01.02    NACL-����    06.07.14  ���Ŷ�ʬ�֤������ե饰���ɲ�
      *  03.05.01    NACL-����    07.06.01  ���롼�׿����б�
      *  04.03.01    NACL-����    08/07/14  �����������ե饰���ɲ�
      *  04.03.02    NACL-����    08/09/25  �����ս������谷���ɲ�
      *
      *  04.04.01    NACL-����    09/02/04  �����ʣ£Σ����ˤ��ɲ�
      *  04.04.02    NACL-����    10/03/29  �������ǡʣ����ܰʹ߸��������
      *                                     ���ɲ�
      *  04.04.03    NACL-����    10/04/06  �����б������ɲ�
      *
      *  04.05.01    NACL-ƣ��    09/07/14  �������ʲ������б�
      *  04.05.02    NACL-����    10/01/19  �޶�ʬ�ɲ�
      *  04.05.03    NACL-����    12/04/10  �����б������ɲ�
      *
      *  04.07.01    NACL-����    12/09/25  �����Ⱥ��������ɲ�
      *  04.07.02    NACL-����    13/01/21  ϫ�ҥ쥻���б�
      *  04.07.03    NACL-����    13/02/12  ϫ�ҥ쥻���б�
      *                                     �ޡ������ˡ��ޡʶ�ۡ�
      *                                     ������ɽ���ֹ�ݶ�ʬ�ֹ��ɲ�
      *  04.07.04    NACL-����    14/04/07  �����б������ɲ�
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
      *01  RECE84-REC.
           03  RECE84-KEY.
               05  RECE84-KEY1.
      *���ŵ��أɣ�
                   07  RECE84-HOSPNUM     PIC  9(02).
      *����ǯ��
                   07  RECE84-SRYYM       PIC  9(06).
      *������ʬ
                   07  RECE84-NYUGAIKBN   PIC  X(01).
      *���ԣɣ�
                   07  RECE84-PTID        PIC  9(10).
      *�쥻�ץȲ�
                   07  RECE84-RECEKA      PIC  X(02).
      *�ݸ��ɣ�        
                   07  RECE84-HKNID       PIC  9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE84-TEKSTYMD    PIC  X(08).
      *���ֹ�
               05  RECE84-ZAINUM          PIC  9(08).
      *���Ŷ�ʬ
           03  RECE84-SRYKBN              PIC  X(02).
      *������ɽ���ֹ�ݶ�ʬ�ֹ�
           03  RECE84-CDKBN-KBNNUM        PIC  9(03).
      *
           03  RECE84-DATA-AREA. 
      *���ż��̶�ʬ
               05  RECE84-SRYSYUKBN           PIC  X(03).
      *�޼��̶�ʬ
               05  RECE84-ZAISKBKBN           PIC  X(01).
      *�����ֹ�
               05  RECE84-PTNUM               PIC  X(20).
      *���Ų�
               05  RECE84-SRYKA               PIC  X(02).
      *�ݸ��ȹ礻�ֹ�
               05  RECE84-HKNCOMBI            PIC  9(04).
      *���ݶ�ʬ
               05  RECE84-JIHOKBN             PIC  X(01).
      *������
               05  RECE84-ZAITEN              PIC  9(08).
      *���ť����ɷ�
               05  RECE84-SRYCDTOTAL          PIC  9(14).
      *���̷�
               05  RECE84-SURYOUTOTAL         PIC  9(07)V9(05).
      *���ť����ɡ����̤��߷�
               05  RECE84-RUIKEITOTAL         PIC  9(13)V9(05).
      *���ٿ�
               05  RECE84-MEISAISU            PIC  9(04).
      *�޲��
               05  RECE84-ZAIKAISU            PIC  9(04).
      *�굻������
               05  RECE84-SYUTEN1             PIC  9(07).
      *�굻������
               05  RECE84-SYUTEN2             PIC  9(07).
      *��������
               05  RECE84-YKZTEN              PIC  9(07).
      *�������
               05  RECE84-KIZAITEN            PIC  9(07).
      *�ɥ�����������
               05  RECE84-DRCD                PIC  X(05).
      *������ե饰
               05  RECE84-ZAIREQFLG           PIC  X(01).
      *����Ψ
               05  RECE84-TEIGENRATE          PIC  X(01).
      *�����ȥ����ɥե饰
               05  RECE84-COMCDFLG            PIC  X(01).
      *�ʥ��˥ե饰
               05  RECE84-SAFLG               PIC  X(01).
      *�����������ʣ����ϣ��ʥ����ȤΤߡˡ�
               05  RECE84-TOYAKUFLG           PIC  X(01).
      *���Ŷ�ʬ�֤������ե饰�ʼ����ե���ѡ�
               05  RECE84-OKIKAEFLG           PIC  X(01).
      *�������������������������ٿ�
               05  RECE84-RYOTENKISAISU       PIC  9(04).
      *�����ս������谷
               05  RECE84-JIBAISYUKEIKBN      PIC  9(01).
      *�ط�������¸�ߥե饰
               05  RECE84-SETCOMFLG           PIC  X(01).
      *���򼰥�����¸�ߥե饰
               05  RECE84-SENTAKUCOMFLG       PIC  X(01).
      *�޶�ʬ
               05  RECE84-ZAIKBN              PIC  9(03).
      *�������ǡʣ����ܰʹ߸��������
               05  RECE84-GAZOGEN             PIC  X(01).
      *�ƿǲû��������ˤκ�
               05  RECE84-NYUSAIFLG           PIC  X(01).
      *�����߰ʲ����ޥ쥻�ץȵ��ܥե饰
               05  RECE84-YKZKISAIFLG         PIC  X(01).
      *��������ʬ
               05  RECE84-JISANYAKKBN         PIC  X(01).
      *
      *��ϥӥ�
               05  RECE84-RIHABIRI-COMMENT    PIC  X(01).
      *��ϥӥ�ʿ����ɼ�����
               05  RECE84-SINDAI-COMMENT      PIC  X(01).
      *��ϥӥ�ʸƵ۴��
               05  RECE84-KOKYUKI-COMMENT     PIC  X(01).
      *��ϥӥ��Ǿ��ɼ�������
               05  RECE84-NOUKE-COMMENT       PIC  X(01).
      *��ϥӥ�ʱ�ư���
               05  RECE84-UNDOKI-COMMENT      PIC  X(01).
      *��ϥӥ�����´��ԡ�
               05  RECE84-NANBYO-COMMENT      PIC  X(01).
      *��ϥӥ�ʾ㳲���ʼԡˡ�
               05  RECE84-SYOGAI-COMMENT      PIC  X(01).
      *��ϥӥ���ݿ���
               05  RECE84-SESSYOKU-COMMENT    PIC  X(01).
      *��ϥӥ�����Ѿɸ�����
               05  RECE84-HAIYO-COMMENT       PIC  X(01).
      *��ã����
               05  RECE84-KAITATU-COMMENT     PIC  X(01).
      *
      *��ϥӥ�û��ʿ����ɼ�����
               05  RECE84-SINDAI9-COMMENT     PIC  X(01).
      *��ϥӥ�û��ʸƵ۴��
               05  RECE84-KOKYUKI9-COMMENT    PIC  X(01).
      *��ϥӥ�û������Ѿɸ�����
               05  RECE84-HAIYO9-COMMENT      PIC  X(01).
      *
      *���޴�����Ƴ��
               05  RECE84-FLG-YAKUKAN-COMMENT PIC  X(01).
      *�������ܿ�����Ƴ��
               05  RECE84-FLG-NYUEIYO-COMMENT PIC  X(01).
      *�ౡ��ˬ���Ƴ��
               05  RECE84-FLG-HOUMON-COMMENT  PIC  X(01).
      *���¼�����ո��������
               05  RECE84-FLG-SYOBYO-COMMENT  PIC  X(01).
      *������Ʊ�ս������
               05  RECE84-FLG-RYOYO-COMMENT   PIC  X(01).
      *�ౡ��������Ƴ�û�����������޴�����Ƴ��
               05  RECE84-FLG-TAIIN-COMMENT   PIC  X(01).
      *ˬ��Ǹ�ؼ���
               05  RECE84-FLG-HOUSIJI-COMMENT PIC  X(01).
      *��ѡ�͢��Ͻ�����
               05  RECE84-FLG-SHUJUTU-COMMENT PIC  X(01).
      *�ãԡ��ͣң�
               05  RECE84-FLG-CTMRI-COMMENT   PIC  X(01).
      *�������ౡ��ˬ���Ƴ��
               05  RECE84-FLG-HOUSEI-COMMENT  PIC  X(01).
      *�����ʳ�Υ�¼������û����������û���
               05  RECE84-FLG-KAKURI-COMMENT  PIC  X(01).
      *ϫ�Ҽ��ŷײ�û����������û���
               05  RECE84-FLG-CHIRYO-COMMENT  PIC  X(01).
      *������Ʃ���Ѳ��Ƴ������
               05  RECE84-FLG-HINKAI-COMMENT  PIC  X(01).
      *����
               05  RECE84-FLG-SANSO-COMMENT   PIC  X(01).
      *��ǡ�Ʊ��ʣ�����Ųʡ�
               05  RECE84-FLG-SYODOU-COMMENT  PIC  X(01).
               05  RECE84-SYODOU-SRYKA-AREA.
                   07  RECE84-SYODOU-SRYKA    PIC  X(02)     OCCURS  10.
      *�������»�Ƚ��
               05  RECE84-KNSJISKBN           PIC  9(01).
      *�������»�Ƚ�ǥ��롼��
               05  RECE84-KNSJISGRPKBN        PIC  9(02).
      *���ǡ����̱���
               05  RECE84-FLG-OSIN-COMMENT    PIC  X(01).
      *�߻ٱ�˴ؤ������
               05  RECE84-FLG-ZAISIEN-COMMENT PIC  X(01).
      *�쥻�����������û��ե饰
               05  RECE84-NYUKSNFLG           PIC  9(01).
      *�����ʣ£ΣС��Σԡݣ���£ΣС��ȣ��ΣС�
               05  RECE84-FLG-KENSA-COMMENT   PIC  X(01).
      *
      *���ٱ�Ϣ�Ȼ�Ƴ��
               05  RECE84-FLG-KAISIEN-COMMENT PIC  X(01).
      *���ı��ܿ�����Ƴ��
               05  RECE84-FLG-SYUEIYO-COMMENT PIC  X(01).
      *�͹���¡��
               05  RECE84-FLG-JINZOU-COMMENT  PIC  X(01).
      *�ɽ걢���ĺ�����
               05  RECE84-FLG-KYOKUIN-COMMENT PIC  X(01).
      *���
               05  RECE84-FLG-MASUI-COMMENT   PIC  X(01).
      *���ܥ��ݡ��ȥ�����û�
               05  RECE84-FLG-EISAPO-COMMENT  PIC  X(01).
      *�Ƶۥ���������û�
               05  RECE84-FLG-KOKKEA-COMMENT  PIC  X(01).
      *�žɼԲû�����������������������
               05  RECE84-FLG-JYUSYO-COMMENT  PIC  X(01).
      *
      *�������ǥե����
               05  RECE84-FLG-FILM-COMMENT    PIC  X(01).
      *
      *������ӽ��۰����ؼ���
               05  RECE84-FLG-KAISYOKU-COMMENT
                                          PIC  X(01).
      *�����ʥ��硼�ȡ������������ʥǥ�������
               05  RECE84-FLG-SEIKEA-COMMENT  PIC  X(01).
      *�����ʥꥨ���������û�
               05  RECE84-FLG-SEIRIE-COMMENT  PIC  X(01).
      *�������޶�̳�»ܲû�
               05  RECE84-FLG-BYOYAK-COMMENT  PIC  X(01).
      *
      *�����³�۰��Ƶ���ˡ��Ƴ������
               05  RECE84-FLG-YOUATU-COMMENT  PIC  X(01).
      *
      *��¡�ڡ����᡼������Ƴ�����������ѷ���ư����ư���
      *��¡�ڡ����᡼������Ƴ�������ʥ�������������ư��ܹԴ��û��ޤ��
               05  RECE84-FLG-SINPESUR-COMMENT  PIC  X(01).
      *��֥�˥���󥰲û�
               05  RECE84-FLG-SINPESU-COMMENT   PIC  X(01).
      *
      *���󴵼Ի�Ƴ������������
               05  RECE84-FLG-GANSIDO-COMMENT   PIC  X(01).
      *���𴵼Զ�Ʊ������
               05  RECE84-FLG-ZAIKYO-COMMENT    PIC  X(01).
      *�����ˤ��������ɷ�ˡ
               05  RECE84-FLG-BOKOHO-COMMENT    PIC  X(01).
      *Ķ�žɻ��ʼԡ��������Ųû�����Ķ�žɻ��ʼԡ��������Ųû�
               05  RECE84-FLG-CYOJKSN-COMMENT   PIC  X(01).
      *��Ǣ��Ω��Ƴ��
               05  RECE84-FLG-HAIJRT-COMMENT    PIC  X(01).
      *����Ⱦ�Ƿ����ܷдɱ���ˡ��Ƴ������
               05  RECE84-FLG-ZAIHANKO-COMMENT  PIC  X(01).
      *�ⵤ�����Ǽ���
               05  RECE84-FLG-KIATU-COMMENT     PIC  X(01).
      *������������ץ������û�
               05  RECE84-FLG-SCPRO-COMMENT     PIC  X(01).
      *
      *���ֳ��۵ޱ��⸡���û��������®���θ����û�
               05  RECE84-FLG-KENKSN-COMMENT    PIC  X(01).
      *���ֳ��۵ޱ���������ǲû�
               05  RECE84-FLG-KINGAZ-COMMENT    PIC  X(01).
      *
      *�ౡ����Ʊ��Ƴ����
               05  RECE84-FLG-TAIKYO-COMMENT    PIC  X(01).
      *
      *�͹��⼪����Ĵ���û�
               05  RECE84-FLG-JINNAI-COMMENT    PIC  X(01).
      *
      *��Ǣ��Ω�ٱ�û�
               05  RECE84-FLG-HAIJSK-COMMENT    PIC  X(01).
      *
      *�굻������ơʼ�ѡ��������ǡ����֡�
               05  RECE84-ACCTP-SYUGI-TBL-G.
                   07    RECE84-ACCTP-SYUGI-TBL     OCCURS  5.
      *�굻���ϥ�����
                         09  RECE84-ACCTP-SRYCD     PIC X(09).
      *�굻����
                         09  RECE84-ACCTP-SYUTEN    PIC 9(07).
      *��������
                         09  RECE84-ACCTP-YKZTEN    PIC 9(07).
      *����¾��������ʥե���ࡦ���ǡ����ǰʳ���
                         09  RECE84-ACCTP-KIZAITEN  PIC 9(07).
      *�ե��������
                         09  RECE84-ACCTP-FIRTEN    PIC 9(07).
      *��������
               05  RECE84-ACCTP-SANSOTEN            PIC 9(07).
      *��������
               05  RECE84-ACCTP-CHISOTEN            PIC 9(07).
      *
      *������ʬ��������
               05  RECE84-HOKATU-TENSU-AREA.
      *������
                   07    RECE84-HOKATU-ZAITEN       PIC 9(08).
      *�굻������
                   07    RECE84-HOKATU-SYUTEN1      PIC 9(07).
      *�굻������
                   07    RECE84-HOKATU-SYUTEN2      PIC 9(07).
      *��������
                   07    RECE84-HOKATU-YKZTEN       PIC 9(07).
      *�������
                   07    RECE84-HOKATU-KIZAITEN     PIC 9(07).
      *
      *�ޡ�������
               05  RECE84-ZAI-TENSU                 PIC 9(09).
      *�ޡʶ�ۡ�
               05  RECE84-ZAI-KINGAKU               PIC 9(09).
      *
      *��������ơ��֥�
               05  RECE84-DAY-AREA.
      *        ���Ų���ơ��֥��1:���,2:����,3:����, 4:����ʾ��
                  07  RECE84-DAY-TBL           OCCURS  4.
                      09  RECE84-KAISUINF      OCCURS  31.
                          11  RECE84-KAISU     PIC  9(03).
      *�쥻�ץȼ���
               05  RECE84-RECESYUBETU          PIC 9(04).