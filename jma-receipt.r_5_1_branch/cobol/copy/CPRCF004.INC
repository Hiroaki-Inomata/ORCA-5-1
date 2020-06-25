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
      *    �쥻�ץ����ٽ�     COPY (CPRCF04.INC)        *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     01.06.29  �ݸ����ֹ桦
      *                                     Ŭ�ѳ���ǯ�������ɲ�
      *  01.00.02    NACL-ƣ��    02.09.02  �����Υơ��֥���ѹ�
      *  01.00.03    NACL-ƣ��    03/03/12  �����ʬ���ɲ�
      *  01.00.04    NACL-����    03.03.03  ���ֹ�彤��
      *  01.00.05    NACL-����    03.09.24  �������Խ��ե饰�ɲ�
      *  01.00.06    NACL-����    04.02.05  ���ť����ɡ����̤��߷�
      *                                     �ɲ�
      *  ��������ǯ��������б�
      *  01.01.01    NACL-����    04.04.05  ������Ʃ���Ѳ��Ƴ������
      *                                     ���ɲ�
      *  01.01.02    NACL-����    04.04.10  ���𴵼�ˬ����ũ���ʹ�����
      *                                     Ƴ�����������ũ���ͤ��ɲ�
      *  01.01.03    NACL-����    05.01.17  ���Ǥ��ɲ�
      *  01.01.04    NACL-����    05.02.28  �ʥ��˥ե饰���ɲ�
      *
      *  02.09.01    NACL-����    06.04.17  ��������ǯ��������б�
      *  03.00.01    NACL-����    06.06.22  ���ǡ����̱��ǡ�
      *                                     ���𴵼�ˬ����������ɲ�
      *  03.01.01    NACL-����    06.07.11  �߻ٱ�˴ؤ�����Ť��ɲ�
      *
      *  03.03.01    NACL-ƣ��    06/09/29  ������Ƚ�����ε�����ˡ���ѹ�
      *                                     �������»�Ƚ�Ǥ��ɲ�
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *  04.02.01    NACL-����    08/03/28  ����������������󥻥�󥰤��ɲ�
      *  04.03.01    NACL-����    08/07/14  �����������ե饰���ɲ�
      *
      *  04.04.01    NACL-����    09/02/04  �����ʣ£Σ����ˤ��ɲ�
      *  04.04.02    NACL-����    10/03/23  Ʊ�����úƿǥե饰���ɲ�
      *  04.04.03    NACL-����    10/03/29  �������ǡʣ����ܰʹ߸��������
      *                                     ���ɲ�
      *  04.04.04    NACL-����    10/04/06  �����б������ɲ�
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
      *  04.06.03    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *
      *  04.07.01    NACL-����    12/09/25  �����Ⱥ��������ɲ�
      *  04.07.02    NACL-����    14/04/07  �����б������ɲ�
      *  04.07.03    NACL-����    14/09/01  ˬ����Ťˤ����뵭Ͽ�񥳥���
      *                                     �ե饰�ɲ�
      *
      *  04.08.01    NACL-����    13/01/22  ��������ơ��֥룱�����б�
      *  04.08.02    NACL-����    13/01/25  ������ʬ�������ѥ쥻�ץȵ����б�
      *  04.08.03    NACL-����    16/02/05  ���ݥ쥻�ץȡ�¾�ݸ��ˤƿǻ�������
      *                                     �ѡץ������Ե����б�
      *  04.08.04    NACL-����    16/03/31  ʿ������ǯ��������б�
      *  04.08.05    NACL-����    16/08/02  �����߰ʲ����ޤ�����ѥ쥻�ץȵ���
      *                                     �ʵ�Ͽ���б�
      *  04.08.06    NACL-����    18/04/02  ʿ������ǯ��������б�
      *  04.08.07    NACL-����    18/05/07  ʿ������ǯ��������б�
      *  04.08.08    NACL-����    18/09/27  ���򼰥������б�
      *  05.00.01    NACL-����    20/04/07  ���£�ǯ��������б�
      ************************************************************
      *01  RECE04-REC.
           03  RECE04-KEY.
               05  RECE04-KEY1.
      *���ŵ��أɣ�
                   07  RECE04-HOSPNUM     PIC 9(2).
      *����ǯ��
                   07  RECE04-SRYYM       PIC 9(06).
      *������ʬ
                   07  RECE04-NYUGAIKBN   PIC X(01).
      *���ԣɣ�
                   07  RECE04-PTID        PIC 9(10).
      *�쥻�ץȲ�
                   07  RECE04-RECEKA      PIC X(02).
      *������ʧ����
                   07  RECE04-TEISYUTUSAKI
                                          PIC 9(01).
      *�쥻�ץȼ���
                   07  RECE04-RECESYUBETU PIC 9(04).
      *�ݸ����ֹ�        
                   07  RECE04-HKNJANUM    PIC X(08).
      *�����ʬ
                   07  RECE04-HOJOKBN     PIC X(01).
      *����ɣ�
                   07  RECE04-KOHID       PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE04-TEKSTYMD    PIC X(08). 
      *ʬ���ʬ
                   07  RECE04-RECEIPT-KBN PIC X(10).
      *���ֹ�
               05  RECE04-ZAINUM          PIC  9(08).
      *����ʬΥ��ʬ
           03  RECE04-KOHBUNRI            PIC  9(01).
      *���Ŷ�ʬ
           03  RECE04-SRYKBN              PIC  X(02).
      *���ż��̶�ʬ
           03  RECE04-SRYSYUKBN           PIC  X(03).
      *�����ֹ�
           03  RECE04-PTNUM               PIC  X(20).
      *���Ų�
           03  RECE04-SRYKA               PIC  X(02).
      *�ݸ��ȹ礻�ֹ�
           03  RECE04-HKNCOMBI            PIC  9(04).
      *���ݶ�ʬ
           03  RECE04-JIHOKBN             PIC  X(01).
      *������
           03  RECE04-ZAITEN              PIC  9(08).
      *���ť����ɷ�
           03  RECE04-SRYCDTOTAL          PIC  9(14).
      *���̷�
           03  RECE04-SURYOUTOTAL         PIC  9(07)V9(05).
      *���ť����ɡ����̤��߷�
           03  RECE04-RUIKEITOTAL         PIC  9(13)V9(05).
      *���ٿ�
           03  RECE04-MEISAISU            PIC  9(04).
      *�޲��
           03  RECE04-ZAIKAISU            PIC  9(04).
      *�굻������
           03  RECE04-SYUTEN1             PIC  9(07).
      *�굻������
           03  RECE04-SYUTEN2             PIC  9(07).
      *��������
           03  RECE04-YKZTEN              PIC  9(07).
      *�������
           03  RECE04-KIZAITEN            PIC  9(07).
      *�ɥ�����������
           03  RECE04-DRCD                PIC  X(05).
      *������ե饰
           03  RECE04-ZAIREQFLG           PIC  X(01).
      *����Ψ
           03  RECE04-TEIGENRATE          PIC  X(01).
      *�����ȥ����ɥե饰
           03  RECE04-COMCDFLG            PIC  X(01).
      *�ʥ��˥ե饰
           03  RECE04-SAFLG               PIC  X(01).
      *�����������ʣ����ϣ��ʥ����ȤΤߡˡ�
           03  RECE04-TOYAKUFLG           PIC  X(01).
      *�ط�������¸�ߥե饰
           03  RECE04-SETCOMFLG           PIC  X(01).
      *���򼰥�����¸�ߥե饰
           03  RECE04-SENTAKUCOMFLG       PIC  X(01).
      *�޶�ʬ
           03  RECE04-ZAIKBN              PIC  9(03).
      *�������ǡʣ����ܰʹ߸��������
           03  RECE04-GAZOGEN             PIC  X(01).
      *ˬ����Ťˤ����뵭Ͽ�񥳥��ȥե饰
           03  RECE04-HOMONCOMFLG         PIC  X(01).
      *¾�ݸ��ˤƿǻ���������Ե��ܥե饰
           03  RECE04-TAHOKENNGFLG        PIC  X(01).
      *����������������Ķ��ͳ�����ȥե饰
           03  RECE04-SIPPUCOMFLG         PIC  X(01).
      *�������ե饰�ʣ�������ʬ����������ʬ��
           03  RECE04-SIPPUFLG            PIC  X(01).
      *�����߰ʲ����ޥ쥻�ץȵ��ܥե饰
           03  RECE04-YKZKISAIFLG         PIC  X(01).
      *
      *���������ʡ�
           03  RECE04-SYOSANTEIKA-COMMENT PIC  X(01).
      *�ƿ�������ʡ�
           03  RECE04-SAISANTEIKA-COMMENT PIC  X(01).
      *���úƿ�
           03  RECE04-DENWASAI-COMMENT    PIC  X(01).
      *���úƿǡʣ���ǯ���������³��
           03  RECE04-DENWASAI2-COMMENT   PIC  X(01).
      *Ʊ���ƿ�
           03  RECE04-DOUJITUSAI-COMMENT  PIC  X(01).
      *Ʊ�����úƿ�
           03  RECE04-DOUDENSAI-COMMENT   PIC  X(01).
      *Ʊ�����úƿǡʣ���ǯ���������³��
           03  RECE04-DOUDENSAI2-COMMENT  PIC  X(01).
      *�ƿǡʣ����ܰʹߡ��͹���¡��
           03  RECE04-SAIJINKO-COMMENT    PIC  X(01).
      *�͹���¡
           03  RECE04-JINKOJIN-COMMENT    PIC  X(01).
      *��ϥӥ�
           03  RECE04-RIHABIRI-COMMENT    PIC  X(01).
      *��ϥӥ�ʿ����ɼ�����
           03  RECE04-SINDAI-COMMENT      PIC  X(01).
      *��ϥӥ�ʸƵ۴��
           03  RECE04-KOKYUKI-COMMENT     PIC  X(01).
      *��ϥӥ��Ǿ��ɼ�������
           03  RECE04-NOUKE-COMMENT       PIC  X(01).
      *��ϥӥ�ʱ�ư���
           03  RECE04-UNDOKI-COMMENT      PIC  X(01).
      *��ϥӥ�����´��ԡ�
           03  RECE04-NANBYO-COMMENT      PIC  X(01).
      *��ϥӥ�ʾ㳲���ʼԡˡ�
           03  RECE04-SYOGAI-COMMENT      PIC  X(01).
      *��ϥӥ���ݿ���
           03  RECE04-SESSYOKU-COMMENT    PIC  X(01).
      *��ϥӥ�����Ѿɸ�����
           03  RECE04-HAIYO-COMMENT       PIC  X(01).
      *��ã����
           03  RECE04-KAITATU-COMMENT     PIC  X(01).
      *
      *��ϥӥ�û��ʿ����ɼ�����
           03  RECE04-SINDAI9-COMMENT     PIC  X(01).
      *��ϥӥ�û��ʸƵ۴��
           03  RECE04-KOKYUKI9-COMMENT    PIC  X(01).
      *��ϥӥ�û������Ѿɸ�����
           03  RECE04-HAIYO9-COMMENT      PIC  X(01).
      *
      *�������˼���������
           03  RECE04-FLG-SHIDOU-COMMENT  PIC  X(01).
      *���¼�����ո��������
           03  RECE04-FLG-SYOBYO-COMMENT  PIC  X(01).
      *������Ʊ�ս������
           03  RECE04-FLG-RYOYO-COMMENT   PIC  X(01).
      *ˬ��Ǹ�ؼ���
           03  RECE04-FLG-HOUSIJI-COMMENT PIC  X(01).
      *��ѡ�͢��Ͻ�����
           03  RECE04-FLG-SHUJUTU-COMMENT PIC  X(01).
      *�ãԡ��ͣң�
           03  RECE04-FLG-CTMRI-COMMENT   PIC  X(01).
      *������Ʃ���Ѳ��Ƴ������
           03  RECE04-FLG-HINKAI-COMMENT  PIC  X(01).
      *���𴵼�ˬ����ũ���ʹ�����Ƴ�����������ũ����
           03  RECE04-FLG-HOUTEN-COMMENT  PIC  X(01).
      *����
           03  RECE04-FLG-SANSO-COMMENT   PIC  X(01).
      *��ǡ�Ʊ��ʣ�����Ųʡ�
           03  RECE04-FLG-SYODOU-COMMENT  PIC  X(01).
           03  RECE04-SYODOU-SRYKA-AREA.
               05  RECE04-SYODOU-SRYKA    PIC  X(02)     OCCURS  10.
      *�ƿǡ�Ʊ���������ܡ�
           03  RECE04-FLG-SAIDOU-COMMENT  PIC  X(01).
           03  RECE04-SAIDOU-SRYKA-AREA.
               05  RECE04-SAIDOU-SRYKA    PIC  X(02)     OCCURS  10.
      *�˥������¸�ɴ�����
           03  RECE04-FLG-NIKOCHIN-COMMENT PIC X(01).
      *������©���Լ��Ŵ����û�
           03  RECE04-FLG-ZENSOKU-COMMENT PIC  X(01).
      *�������»�Ƚ��
           03  RECE04-KNSJISKBN           PIC  9(01).
      *�������»�Ƚ�ǥ��롼��
           03  RECE04-KNSJISGRPKBN        PIC  9(02).
      *���ǡ����̱���
           03  RECE04-FLG-OSIN-COMMENT    PIC  X(01).
      *���𴵼�ˬ�������
           03  RECE04-FLG-ZAIHOU-COMMENT  PIC  X(01).
      *�߻ٱ�˴ؤ������
           03  RECE04-FLG-ZAISIEN-COMMENT PIC  X(01).
      *����������������󥻥��
           03  RECE04-FLG-SYOUNI-COMMENT  PIC  X(01).
      *�����ʣ£ΣС��Σԡݣ���£ΣС��ȣ��ΣС�
           03  RECE04-FLG-KENSA-COMMENT   PIC  X(01).
      *
      *��¡�ڡ����᡼������Ƴ�������ʱ�֥�˥���󥰡�
           03  RECE04-FLG-SINPESU-COMMENT PIC  X(01).
      *���ı��ܿ�����Ƴ��
           03  RECE04-FLG-SYUEIYO-COMMENT PIC  X(01).
      *�͹���¡��
           03  RECE04-FLG-JINZOU-COMMENT  PIC  X(01).
      *�ɽ걢���ĺ�����
           03  RECE04-FLG-KYOKUIN-COMMENT PIC  X(01).
      *���
           03  RECE04-FLG-MASUI-COMMENT   PIC  X(01).
      *ǧ����ˡ��ǧ�ι�ư��ˡ
           03  RECE04-FLG-NINRYO-COMMENT  PIC  X(01).
      *
      *�������ǥե����
           03  RECE04-FLG-FILM-COMMENT    PIC  X(01).
      *
      *ǧ�ξ�������Ǵ�������¾
           03  RECE04-FLG-NINSEN-COMMENT  PIC  X(01).
      *�ౡ����Ʊ��Ƴ�����ʣ��ʳ���
           03  RECE04-FLG-TAKYO-COMMENT   PIC  X(01).
      *�ౡ����Ʊ��Ƴ�����̴�����Ƴ�û�
           03  RECE04-FLG-TAKYOKSN-COMMENT
                                          PIC  X(01).
      *�����ϥӥ�ơ������������������������ȼͿ�����
           03  RECE04-FLG-GAIRIHA-COMMENT PIC  X(01).
      *������ӽ��۰����ؼ���
           03  RECE04-FLG-KAISYOKU-COMMENT
                                          PIC  X(01).
      *
      *�����³�۰��Ƶ���ˡ��Ƴ������
           03  RECE04-FLG-YOUATU-COMMENT  PIC  X(01).
      *
      *��¡�ڡ����᡼������Ƴ�����������ѷ���ư����ư���
      *��¡�ڡ����᡼������Ƴ�������ʥ�������������ư��ܹԴ��û��ޤ��
           03  RECE04-FLG-SINPESUR-COMMENT
                                          PIC  X(01).
      *���󴵼Ի�Ƴ������������
           03  RECE04-FLG-GANSIDO-COMMENT PIC  X(01).
      *���𴵼Զ�Ʊ������
           03  RECE04-FLG-ZAIKYO-COMMENT  PIC  X(01).
      *���𴵼�ˬ���ϥӥ�ơ�������Ƴ������
           03  RECE04-FLG-ZAIRIHA-COMMENT PIC  X(01).
      *�����ˤ��������ɷ�ˡ
           03  RECE04-FLG-BOKOHO-COMMENT  PIC  X(01).
      *�̱�������������ˡ�ʣ�����̤���˲û�
           03  RECE04-FLG-TUINZAI-COMMENT PIC  X(01).
           03  RECE04-FLG-TUINZAI2-COMMENT
                                          PIC  X(01).
      *��¸�ɽ�����ˡ
           03  RECE04-FLG-IZONSYO-COMMENT PIC  X(01).
      *���𴵼�ˬ�����޴�����Ƴ��
           03  RECE04-FLG-ZAIYAK-COMMENT  PIC  X(01).
      *������žɲ�ͽ�ɥ�������Ƴ��
           03  RECE04-FLG-NYUSEN-COMMENT  PIC  X(01).
      *�ⵤ�����Ǽ���
           03  RECE04-FLG-KIATU-COMMENT   PIC  X(01).
      *
      *����饤�������
           03  RECE04-FLG-ONLINE-COMMENT  PIC  X(01).
           03  RECE04-FLG-ONLINE2-COMMENT PIC  9(02).
           03  RECE04-FLG-ONLINE3-COMMENT PIC  X(01).
      *
      *�͹��⼪����Ĵ���û�
           03  RECE04-FLG-JINNAI-COMMENT  PIC  X(01).
      *
      *������Ǣ��Ω��Ƴ��
           03  RECE04-FLG-GAIJIR-COMMENT  PIC  X(01).
      *
      *��������ơ��֥�
           03  RECE04-DAY-AREA.
      *        ���Ų���ơ��֥��1:��ס�2-9:����������
             04  RECE04-DAY-TBL            OCCURS  10.
                 05  RECE04-KAISUINF       OCCURS  31.
                     07  RECE04-KAISU      PIC  9(03).
      *
      *�굻������ơʼ�ѡ��������ǡ����֡�
           03  RECE04-ACCTP-SYUGI-TBL-G.
               05  RECE04-ACCTP-SYUGI-TBL  OCCURS  5.
      *�굻���ϥ�����
                   07  RECE04-ACCTP-SRYCD  PIC X(09).
      *�굻����
                   07  RECE04-ACCTP-SYUTEN PIC 9(07).
      *��������
                   07  RECE04-ACCTP-YKZTEN PIC 9(07).
      *����¾��������ʥե���ࡦ���ǡ����ǰʳ���
                   07  RECE04-ACCTP-KIZAITEN
                                           PIC 9(07).
      *�ե��������
                   07  RECE04-ACCTP-FIRTEN PIC 9(07).
      *��������
           03  RECE04-ACCTP-SANSOTEN       PIC 9(07).
      *��������
           03  RECE04-ACCTP-CHISOTEN       PIC 9(07).
      *
      *������ʬ��������
           03  RECE04-HOKATU-TENSU-AREA.
      *������
               05  RECE04-HOKATU-ZAITEN    PIC 9(08).
      *�굻������
               05  RECE04-HOKATU-SYUTEN1   PIC 9(07).
      *�굻������
               05  RECE04-HOKATU-SYUTEN2   PIC 9(07).
      *��������
               05  RECE04-HOKATU-YKZTEN    PIC 9(07).
      *�������
               05  RECE04-HOKATU-KIZAITEN  PIC 9(07).