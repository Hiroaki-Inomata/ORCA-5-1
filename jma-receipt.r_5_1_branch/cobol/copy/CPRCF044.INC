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
      *    �쥻�ץ����ٽ�     COPY (CPRCF044.INC)                *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    02.09.20  �����Υơ��֥���ѹ�
      *  01.00.02    NACL-����    03.03.03  ���ֹ�彤��
      *  01.00.03    NACL-����    03.09.24  �������Խ��ե饰�ɲ�
      *  01.00.04    NACL-����    04.02.05  ���ť����ɡ����̤��߷�
      *                                     �ɲ�
      *  ��������ǯ��������б�
      *  01.01.01    NACL-����    04.04.05  ������Ʃ���Ѳ��Ƴ������
      *                                     ���ɲ�
      *  01.01.02    NACL-����    04.04.10  ���𴵼�ˬ����ũ���ʹ�����
      *                                     Ƴ�����������ũ���ͤ��ɲ�
      *  01.01.03    NACL-����    05.01.17  ���Ǥ��ɲ�
      *  01.01.04    NACL-����    05.02.28  �ʥ��˥ե饰���ɲ�
      *  02.09.01    NACL-����    06.04.17  ��������ǯ��������б�
      *  03.00.01    NACL-����    06.06.22  ���ǡ����̱��ǡ�
      *                                     ���𴵼�ˬ����������ɲ�
      *  03.01.01    NACL-����    06.07.11  �߻ٱ�˴ؤ�����Ť��ɲ�
      *  03.01.02    NACL-����    06.07.14  ���Ŷ�ʬ�֤������ե饰���ɲ�
      *  03.04.01    NACL-����    06.12.01  �����շ��ݽ���б�
      *  03.05.01    NACL-����    07/05/28  ���롼�׿����б�
      *  04.02.01    NACL-����    08/03/28  ����������������󥻥�󥰤��ɲ�
      *  04.03.01    NACL-����    08/07/14  �����������ե饰���ɲ�
      *  04.03.02    NACL-����    08/09/25  �����ս������谷���ɲ�
      *
      *  04.04.01    NACL-����    09/02/04  �����ʣ£Σ����ˤ��ɲ�
      *  04.04.02    NACL-����    10/03/23  Ʊ�����úƿǥե饰���ɲ�
      *  04.04.03    NACL-����    10/03/29  �������ǡʣ����ܰʹ߸��������
      *                                     ���ɲ�
      *  04.04.04    NACL-����    10/04/06  �����б������ɲ�
      *
      *  04.05.01    NACL-ƣ��    09/07/14  �������ʲ������б�
      *  04.05.02    NACL-����    10/01/19  �޶�ʬ�ɲ�
      *  04.05.03    NACL-����    12/04/10  �����б������ɲ�
      *
      *  04.07.01    NACL-����    12/09/25  �����Ⱥ��������ɲ�
      *  04.07.02    NACL-����    13/01/21  ϫ�ҥ쥻���б�
      *  04.07.03    NACL-����    13/02/12  ϫ�ҥ쥻���б�
      *                                     �ޡ������ˡ��ޡʶ�ۡ��ɲ�
      *  04.07.04    NACL-����    14/04/07  �����б������ɲ�
      *  04.07.05    NACL-����    15/03/09  ˬ����Ťˤ����뵭Ͽ�񥳥���
      *                                     �ե饰�ɲ�
      *
      *  04.08.01    NACL-����    13/01/22  ��������ơ��֥룱�����б�
      *  04.08.02    NACL-����    13/01/25  ������ʬ�������ѥ쥻�ץȵ����б�
      *  04.08.03    NACL-����    16/03/31  ʿ������ǯ��������б�
      *  04.08.04    NACL-����    16/08/02  �����߰ʲ����ޤ�����ѥ쥻�ץȵ���
      *                                     �ʵ�Ͽ���б�
      *  04.08.05    NACL-����    18/04/02  ʿ������ǯ��������б�
      *  04.08.06    NACL-����    18/05/07  ʿ������ǯ��������б�
      *  04.08.07    NACL-����    18/09/27  ���򼰥������б�
      *  05.00.01    NACL-����    20/04/07  ���£�ǯ��������б�
      ************************************************************
      *01  RECE44-REC.
           03  RECE44-KEY.
               05  RECE44-KEY1.
      *���ŵ��أɣ�
                   07  RECE44-HOSPNUM     PIC  9(02).
      *����ǯ��
                   07  RECE44-SRYYM       PIC  9(06).
      *������ʬ
                   07  RECE44-NYUGAIKBN   PIC  X(01).
      *���ԣɣ�
                   07  RECE44-PTID        PIC  9(10).
      *�쥻�ץȲ�
                   07  RECE44-RECEKA      PIC  X(02).
      *�ݸ��ɣ�        
                   07  RECE44-HKNID       PIC  9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE44-TEKSTYMD    PIC  X(08).
      *���ֹ�
               05  RECE44-ZAINUM          PIC  9(08).
      *���Ŷ�ʬ
           03  RECE44-SRYKBN              PIC  X(02).
      *
           03  RECE44-DATA-AREA. 
      *���ż��̶�ʬ
               05  RECE44-SRYSYUKBN           PIC  X(03).
      *�����ֹ�
               05  RECE44-PTNUM               PIC  X(20).
      *���Ų�
               05  RECE44-SRYKA               PIC  X(02).
      *�ݸ��ȹ礻�ֹ�
               05  RECE44-HKNCOMBI            PIC  9(04).
      *���ݶ�ʬ
               05  RECE44-JIHOKBN             PIC  X(01).
      *������
               05  RECE44-ZAITEN              PIC  9(08).
      *���ť����ɷ�
               05  RECE44-SRYCDTOTAL          PIC  9(14).
      *���̷�
               05  RECE44-SURYOUTOTAL         PIC  9(07)V9(05).
      *���ť����ɡ����̤��߷�
               05  RECE44-RUIKEITOTAL         PIC  9(13)V9(05).
      *���ٿ�
               05  RECE44-MEISAISU            PIC  9(04).
      *�޲��
               05  RECE44-ZAIKAISU            PIC  9(04).
      *�굻������
               05  RECE44-SYUTEN1             PIC  9(07).
      *�굻������
               05  RECE44-SYUTEN2             PIC  9(07).
      *��������
               05  RECE44-YKZTEN              PIC  9(07).
      *�������
               05  RECE44-KIZAITEN            PIC  9(07).
      *�ɥ�����������
               05  RECE44-DRCD                PIC  X(05).
      *������ե饰
               05  RECE44-ZAIREQFLG           PIC  X(01).
      *����Ψ
               05  RECE44-TEIGENRATE          PIC  X(01).
      *�����ȥ����ɥե饰
               05  RECE44-COMCDFLG            PIC  X(01).
      *�ʥ��˥ե饰
               05  RECE44-SAFLG               PIC  X(01).
      *�����������ʣ����ϣ��ʥ����ȤΤߡˡ�
               05  RECE44-TOYAKUFLG           PIC  X(01).
      *���Ŷ�ʬ�֤������ե饰�ʼ����ե���ѡ�
               05  RECE44-OKIKAEFLG           PIC  X(01).
      *�����ս������谷
               05  RECE44-JIBAISYUKEIKBN      PIC  9(01).
      *�ط�������¸�ߥե饰
               05  RECE44-SETCOMFLG           PIC  X(01).
      *���򼰥�����¸�ߥե饰
               05  RECE44-SENTAKUCOMFLG       PIC  X(01).
      *�޶�ʬ
               05  RECE44-ZAIKBN              PIC  9(03).
      *�������ǡʣ����ܰʹ߸��������
               05  RECE44-GAZOGEN             PIC  X(01).
      *ˬ����Ťˤ����뵭Ͽ�񥳥��ȥե饰
               05  RECE44-HOMONCOMFLG         PIC  X(01).
      *����������������Ķ��ͳ�����ȥե饰
               05  RECE44-SIPPUCOMFLG         PIC  X(01).
      *�������ե饰�ʣ�������ʬ����������ʬ��
               05  RECE44-SIPPUFLG            PIC  X(01).
      *�����߰ʲ����ޥ쥻�ץȵ��ܥե饰
               05  RECE44-YKZKISAIFLG         PIC  X(01).
      *
      *���������ʡ�
               05  RECE44-SYOSANTEIKA-COMMENT PIC  X(01).
      *�ƿ�������ʡ�
               05  RECE44-SAISANTEIKA-COMMENT PIC  X(01).
      *���úƿ�
               05  RECE44-DENWASAI-COMMENT    PIC  X(01).
      *���úƿǡʣ���ǯ���������³��
               05  RECE44-DENWASAI2-COMMENT   PIC  X(01).
      *Ʊ���ƿ�
               05  RECE44-DOUJITUSAI-COMMENT  PIC  X(01).
      *Ʊ�����úƿ�
               05  RECE44-DOUDENSAI-COMMENT   PIC  X(01).
      *Ʊ�����úƿǡʣ���ǯ���������³��
               05  RECE44-DOUDENSAI2-COMMENT  PIC  X(01).
      *��ϥӥ�
               05  RECE44-RIHABIRI-COMMENT    PIC  X(01).
      *��ϥӥ�ʿ����ɼ�����
               05  RECE44-SINDAI-COMMENT      PIC  X(01).
      *��ϥӥ�ʸƵ۴��
               05  RECE44-KOKYUKI-COMMENT     PIC  X(01).
      *��ϥӥ��Ǿ��ɼ�������
               05  RECE44-NOUKE-COMMENT       PIC  X(01).
      *��ϥӥ�ʱ�ư���
               05  RECE44-UNDOKI-COMMENT      PIC  X(01).
      *��ϥӥ�����´��ԡ�
               05  RECE44-NANBYO-COMMENT      PIC  X(01).
      *��ϥӥ�ʾ㳲���ʼԡˡ�
               05  RECE44-SYOGAI-COMMENT      PIC  X(01).
      *��ϥӥ���ݿ���
               05  RECE44-SESSYOKU-COMMENT    PIC  X(01).
      *��ϥӥ�����Ѿɸ�����
               05  RECE44-HAIYO-COMMENT       PIC  X(01).
      *��ã����
               05  RECE44-KAITATU-COMMENT     PIC  X(01).
      *
      *��ϥӥ�û��ʿ����ɼ�����
               05  RECE44-SINDAI9-COMMENT     PIC  X(01).
      *��ϥӥ�û��ʸƵ۴��
               05  RECE44-KOKYUKI9-COMMENT    PIC  X(01).
      *��ϥӥ�û������Ѿɸ�����
               05  RECE44-HAIYO9-COMMENT      PIC  X(01).
      *
      *�������˼���������
               05  RECE44-FLG-SHIDOU-COMMENT  PIC  X(01).
      *���¼�����ո��������
               05  RECE44-FLG-SYOBYO-COMMENT  PIC  X(01).
      *������Ʊ�ս������
               05  RECE44-FLG-RYOYO-COMMENT   PIC  X(01).
      *ˬ��Ǹ�ؼ���
               05  RECE44-FLG-HOUSIJI-COMMENT PIC  X(01).
      *��ѡ�͢��Ͻ�����
               05  RECE44-FLG-SHUJUTU-COMMENT PIC  X(01).
      *�ãԡ��ͣң�
               05  RECE44-FLG-CTMRI-COMMENT   PIC  X(01).
      *������Ʃ���Ѳ��Ƴ������
               05  RECE44-FLG-HINKAI-COMMENT  PIC  X(01).
      *���𴵼�ˬ����ũ���ʹ�����Ƴ�����������ũ����
               05  RECE44-FLG-HOUTEN-COMMENT  PIC  X(01).
      *����
               05  RECE44-FLG-SANSO-COMMENT   PIC  X(01).
      *��ǡ�Ʊ��ʣ�����Ųʡ�
               05  RECE44-FLG-SYODOU-COMMENT  PIC  X(01).
               05  RECE44-SYODOU-SRYKA-AREA.
                   07  RECE44-SYODOU-SRYKA    PIC  X(02)     OCCURS  10.
      *�ƿǡ�Ʊ���������ܡ�
               05  RECE44-FLG-SAIDOU-COMMENT  PIC  X(01).
               05  RECE44-SAIDOU-SRYKA-AREA.
                   07  RECE44-SAIDOU-SRYKA    PIC  X(02)     OCCURS  10.
      *�˥������¸�ɴ�����
               05  RECE44-FLG-NIKOCHIN-COMMENT PIC X(01).
      *������©���Լ��Ŵ����û�
               05  RECE44-FLG-ZENSOKU-COMMENT PIC  X(01).
      *�������»�Ƚ��
               05  RECE44-KNSJISKBN           PIC  9(01).
      *�������»�Ƚ�ǥ��롼��
               05  RECE44-KNSJISGRPKBN        PIC  9(02).
      *���ǡ����̱���
               05  RECE44-FLG-OSIN-COMMENT    PIC  X(01).
      *���𴵼�ˬ�������
               05  RECE44-FLG-ZAIHOU-COMMENT  PIC  X(01).
      *�߻ٱ�˴ؤ������
               05  RECE44-FLG-ZAISIEN-COMMENT PIC  X(01).
      *����������������󥻥��
               05  RECE44-FLG-SYOUNI-COMMENT  PIC  X(01).
      *�����ʣ£ΣС��Σԡݣ���£ΣС��ȣ��ΣС�
               05  RECE44-FLG-KENSA-COMMENT   PIC  X(01).
      *
      *��¡�ڡ����᡼������Ƴ�������ʱ�֥�˥���󥰡�
               05  RECE44-FLG-SINPESU-COMMENT PIC  X(01).
      *���ı��ܿ�����Ƴ��
               05  RECE44-FLG-SYUEIYO-COMMENT PIC  X(01).
      *�͹���¡��
               05  RECE44-FLG-JINZOU-COMMENT  PIC  X(01).
      *�ɽ걢���ĺ�����
               05  RECE44-FLG-KYOKUIN-COMMENT PIC  X(01).
      *���
               05  RECE44-FLG-MASUI-COMMENT   PIC  X(01).
      *ǧ����ˡ��ǧ�ι�ư��ˡ
               05  RECE44-FLG-NINRYO-COMMENT  PIC  X(01).
      *
      *�������ǥե����
               05  RECE44-FLG-FILM-COMMENT    PIC  X(01).
      *
      *ǧ�ξ�������Ǵ�������¾
               05  RECE44-FLG-NINSEN-COMMENT  PIC  X(01).
      *�ౡ����Ʊ��Ƴ�����ʣ��ʳ���
               05  RECE44-FLG-TAKYO-COMMENT   PIC  X(01).
      *�ౡ����Ʊ��Ƴ�����̴�����Ƴ�û�
               05  RECE44-FLG-TAKYOKSN-COMMENT
                                           PIC X(01).
      *�����ϥӥ�ơ������������������������ȼͿ�����
               05  RECE44-FLG-GAIRIHA-COMMENT PIC  X(01).
      *������ӽ��۰����ؼ���
               05  RECE44-FLG-KAISYOKU-COMMENT
                                           PIC X(01).
      *
      *�����³�۰��Ƶ���ˡ��Ƴ������
               05  RECE44-FLG-YOUATU-COMMENT
                                           PIC X(01).
      *��������û����ɤ��ؤ���
               05  RECE44-FLG-GAIKAN-COMMENT
                                           PIC X(01).
      *
      *��¡�ڡ����᡼������Ƴ�����������ѷ���ư����ư���
      *��¡�ڡ����᡼������Ƴ�������ʥ�������������ư��ܹԴ��û��ޤ��
               05  RECE44-FLG-SINPESUR-COMMENT PIC  X(01).
      *���󴵼Ի�Ƴ������������
               05  RECE44-FLG-GANSIDO-COMMENT  PIC  X(01).
      *���𴵼Զ�Ʊ������
               05  RECE44-FLG-ZAIKYO-COMMENT   PIC  X(01).
      *���𴵼�ˬ���ϥӥ�ơ�������Ƴ������
               05  RECE44-FLG-ZAIRIHA-COMMENT  PIC  X(01).
      *�����ˤ��������ɷ�ˡ
               05  RECE44-FLG-BOKOHO-COMMENT   PIC  X(01).
      *�̱�������������ˡ�ʣ�����̤���˲û�
               05  RECE44-FLG-TUINZAI-COMMENT  PIC  X(01).
               05  RECE44-FLG-TUINZAI2-COMMENT PIC  X(01).
      *��¸�ɽ�����ˡ
               05  RECE44-FLG-IZONSYO-COMMENT  PIC  X(01).
      *���𴵼�ˬ�����޴�����Ƴ��
               05  RECE44-FLG-ZAIYAK-COMMENT   PIC  X(01).
      *������žɲ�ͽ�ɥ�������Ƴ��
               05  RECE44-FLG-NYUSEN-COMMENT   PIC  X(01).
      *�ⵤ�����Ǽ���
               05  RECE44-FLG-KIATU-COMMENT    PIC  X(01).
      *
      *����饤�������
               05  RECE44-FLG-ONLINE-COMMENT   PIC  X(01).
               05  RECE44-FLG-ONLINE2-COMMENT  PIC  9(02).
               05  RECE44-FLG-ONLINE3-COMMENT  PIC  X(01).
      *
      *�͹��⼪����Ĵ���û�
               05  RECE44-FLG-JINNAI-COMMENT   PIC  X(01).
      *
      *������Ǣ��Ω��Ƴ��
               05  RECE44-FLG-GAIJIR-COMMENT   PIC  X(01).
      *
      *�굻������ơʼ�ѡ��������ǡ����֡�
               05  RECE44-ACCTP-SYUGI-TBL-G.
                   07  RECE44-ACCTP-SYUGI-TBL  OCCURS  5.
      *�굻���ϥ�����
                       09  RECE44-ACCTP-SRYCD  PIC X(09).
      *�굻����
                       09  RECE44-ACCTP-SYUTEN PIC 9(07).
      *��������
                       09  RECE44-ACCTP-YKZTEN PIC 9(07).
      *����¾��������ʥե���ࡦ���ǡ����ǰʳ���
                       09  RECE44-ACCTP-KIZAITEN
                                               PIC 9(07).
      *�ե��������
                       09  RECE44-ACCTP-FIRTEN PIC 9(07).
      *��������
               05  RECE44-ACCTP-SANSOTEN   PIC 9(07).
      *��������
               05  RECE44-ACCTP-CHISOTEN   PIC 9(07).
      *
      *������ʬ��������
               05  RECE44-HOKATU-TENSU-AREA.
      *������
                   07    RECE44-HOKATU-ZAITEN  PIC 9(08).
      *�굻������
                   07    RECE44-HOKATU-SYUTEN1 PIC 9(07).
      *�굻������
                   07    RECE44-HOKATU-SYUTEN2 PIC 9(07).
      *��������
                   07    RECE44-HOKATU-YKZTEN  PIC 9(07).
      *�������
                   07    RECE44-HOKATU-KIZAITEN
                                               PIC 9(07).
      *
      *�ޡ�������
               05  RECE44-ZAI-TENSU        PIC 9(09).
      *�ޡʶ�ۡ�
               05  RECE44-ZAI-KINGAKU      PIC 9(09).
      *
      *��������ơ��֥�
               05  RECE44-DAY-AREA.
      *            ���Ų���ơ��֥��1:��ס�2-9:����������
                    07  RECE44-DAY-TBL         OCCURS  10.
                        09  RECE44-KAISUINF    OCCURS  31.
                            11  RECE44-KAISU   PIC 9(03).
      *�쥻�ץȼ���
               05  RECE44-RECESYUBETU          PIC 9(04).
