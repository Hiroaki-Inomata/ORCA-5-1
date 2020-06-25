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
      ************************************************************
      *    �쥻�ץ����ٽ��ϫ�ҡ�     COPY (CPRCF0841.INC)
      *    (�쥻�ץ��Ż����������ƥ�Υ쥳�����¤��ؤ��ѥ��ԡ����
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.07.01    NACL-����    14/04/07  �����б������ɲ�
      *  04.08.01    NACL-����    16/03/31  ʿ������ǯ��������б�
      *  04.08.02    NACL-����    16/08/02  �����߰ʲ����ޤ�����ѥ쥻�ץȵ���
      *                                     �ʵ�Ͽ���б�
      *  04.08.03    NACL-����    16/08/23  �������б�
      *  04.08.04    NACL-����    18/04/17  ʿ������ǯ��������б�
      *  04.08.05    NACL-����    18/09/27  ���򼰥������б�
      *
      *  05.00.01    NACL-����    19/05/16  �ౡ����Ʊ��Ƴ���������ȼ�ư����
      *                                     �б�
      *  05.00.02    NACL-����    20/04/07  ���£�ǯ��������б�
      ************************************************************
      *01  RECE841-REC.
           03  RECE841-KEY.
               05  RECE841-KEY1.
      *���ŵ��أɣ�
                   07  RECE841-HOSPNUM     PIC  9(02).
      *����ǯ��
                   07  RECE841-SRYYM       PIC  9(06).
      *������ʬ
                   07  RECE841-NYUGAIKBN   PIC  X(01).
      *���ԣɣ�
                   07  RECE841-PTID        PIC  9(10).
      *�쥻�ץȲ�
                   07  RECE841-RECEKA      PIC  X(02).
      *�ݸ��ɣ�        
                   07  RECE841-HKNID       PIC  9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE841-TEKSTYMD    PIC  X(08).
      *���Ŷ�ʬ
               05  RECE841-SRYKBN          PIC  X(02).
      *������ɽ���ֹ�ݶ�ʬ�ֹ�
               05  RECE841-CDKBN-KBNNUM    PIC  9(03).
      *���ֹ�
               05  RECE841-ZAINUM          PIC  9(08).
      *
           03  RECE841-DATA-AREA.
      *���ż��̶�ʬ
               05  RECE841-SRYSYUKBN       PIC  X(03).
      *�޼��̶�ʬ
               05  RECE841-ZAISKBKBN       PIC  X(01).
      *�����ֹ�
               05  RECE841-PTNUM           PIC  X(20).
      *���Ų�
               05  RECE841-SRYKA           PIC  X(02).
      *�ݸ��ȹ礻�ֹ�
               05  RECE841-HKNCOMBI        PIC  9(04).
      *���ݶ�ʬ
               05  RECE841-JIHOKBN         PIC  X(01).
      *������
               05  RECE841-ZAITEN          PIC  9(08).
      *���ť����ɷ�
               05  RECE841-SRYCDTOTAL      PIC  9(14).
      *���̷�
               05  RECE841-SURYOUTOTAL     PIC  9(07)V9(05).
      *���ť����ɡ����̤��߷�
               05  RECE841-RUIKEITOTAL     PIC  9(13)V9(05).
      *���ٿ�
               05  RECE841-MEISAISU        PIC  9(04).
      *�޲��
               05  RECE841-ZAIKAISU        PIC  9(04).
      *�굻������
               05  RECE841-SYUTEN1         PIC  9(07).
      *�굻������
               05  RECE841-SYUTEN2         PIC  9(07).
      *��������
               05  RECE841-YKZTEN          PIC  9(07).
      *�������
               05  RECE841-KIZAITEN        PIC  9(07).
      *�ɥ�����������
               05  RECE841-DRCD            PIC  X(05).
      *������ե饰
               05  RECE841-ZAIREQFLG       PIC  X(01).
      *����Ψ
               05  RECE841-TEIGENRATE      PIC  X(01).
      *�����ȥ����ɥե饰
               05  RECE841-COMCDFLG        PIC  X(01).
      *�ʥ��˥ե饰
               05  RECE841-SAFLG           PIC  X(01).
      *�����������ʣ����ϣ��ʥ����ȤΤߡˡ�
               05  RECE841-TOYAKUFLG       PIC  X(01).
      *���Ŷ�ʬ�֤������ե饰�ʼ����ե���ѡ�
               05  RECE841-OKIKAEFLG       PIC  X(01).
      *�������������������������ٿ�
               05  RECE841-RYOTENKISAISU   PIC  9(04).
      *�����ս������谷
               05  RECE841-JIBAISYUKEIKBN  PIC  9(01).
      *�ط�������¸�ߥե饰
               05  RECE841-SETCOMFLG       PIC  X(01).
      *���򼰥�����¸�ߥե饰
               05  RECE841-SENTAKUCOMFLG   PIC  X(01).
      *�޶�ʬ
               05  RECE841-ZAIKBN          PIC  9(03).
      *�������ǡʣ����ܰʹ߸��������
               05  RECE841-GAZOGEN         PIC  X(01).
      *�ƿǲû��������ˤκ�
               05  RECE841-NYUSAIFLG       PIC  X(01).
      *�����߰ʲ����ޥ쥻�ץȵ��ܥե饰
               05  RECE841-YKZKISAIFLG     PIC  X(01).
      *��������ʬ
               05  RECE841-JISANYAKKBN     PIC  X(01).
      *
      *��ϥӥ�
               05  RECE841-RIHABIRI-COMMENT    PIC  X(01).
      *��ϥӥ�ʿ����ɼ�����
               05  RECE841-SINDAI-COMMENT      PIC  X(01).
      *��ϥӥ�ʸƵ۴��
               05  RECE841-KOKYUKI-COMMENT     PIC  X(01).
      *��ϥӥ��Ǿ��ɼ�������
               05  RECE841-NOUKE-COMMENT       PIC  X(01).
      *��ϥӥ�ʱ�ư���
               05  RECE841-UNDOKI-COMMENT      PIC  X(01).
      *��ϥӥ�����´��ԡ�
               05  RECE841-NANBYO-COMMENT      PIC  X(01).
      *��ϥӥ�ʾ㳲���ʼԡˡ�
               05  RECE841-SYOGAI-COMMENT      PIC  X(01).
      *��ϥӥ���ݿ���
               05  RECE841-SESSYOKU-COMMENT    PIC  X(01).
      *��ϥӥ�����Ѿɸ�����
               05  RECE841-HAIYO-COMMENT       PIC  X(01).
      *��ã����
               05  RECE841-KAITATU-COMMENT     PIC  X(01).
      *
      *��ϥӥ�û��ʿ����ɼ�����
               05  RECE841-SINDAI9-COMMENT     PIC  X(01).
      *��ϥӥ�û��ʸƵ۴��
               05  RECE841-KOKYUKI9-COMMENT    PIC  X(01).
      *��ϥӥ�û������Ѿɸ�����
               05  RECE841-HAIYO9-COMMENT      PIC  X(01).
      *
      *���޴�����Ƴ��
               05  RECE841-FLG-YAKUKAN-COMMENT PIC  X(01).
      *�������ܿ�����Ƴ��
               05  RECE841-FLG-NYUEIYO-COMMENT PIC  X(01).
      *�ౡ��ˬ���Ƴ��
               05  RECE841-FLG-HOUMON-COMMENT  PIC  X(01).
      *���¼�����ո��������
               05  RECE841-FLG-SYOBYO-COMMENT  PIC  X(01).
      *������Ʊ�ս������
               05  RECE841-FLG-RYOYO-COMMENT   PIC  X(01).
      *�ౡ��������Ƴ�û�����������޴�����Ƴ��
               05  RECE841-FLG-TAIIN-COMMENT   PIC  X(01).
      *ˬ��Ǹ�ؼ���
               05  RECE841-FLG-HOUSIJI-COMMENT PIC  X(01).
      *��ѡ�͢��Ͻ�����
               05  RECE841-FLG-SHUJUTU-COMMENT PIC  X(01).
      *�ãԡ��ͣң�
               05  RECE841-FLG-CTMRI-COMMENT   PIC  X(01).
      *�������ౡ��ˬ���Ƴ��
               05  RECE841-FLG-HOUSEI-COMMENT  PIC  X(01).
      *�����ʳ�Υ�¼������û����������û���
               05  RECE841-FLG-KAKURI-COMMENT  PIC  X(01).
      *ϫ�Ҽ��ŷײ�û����������û���
               05  RECE841-FLG-CHIRYO-COMMENT  PIC  X(01).
      *������Ʃ���Ѳ��Ƴ������
               05  RECE841-FLG-HINKAI-COMMENT  PIC  X(01).
      *����
               05  RECE841-FLG-SANSO-COMMENT   PIC  X(01).
      *��ǡ�Ʊ��ʣ�����Ųʡ�
               05  RECE841-FLG-SYODOU-COMMENT  PIC  X(01).
               05  RECE841-SYODOU-SRYKA-AREA.
                   07  RECE841-SYODOU-SRYKA    PIC  X(02)  OCCURS 10.
      *�������»�Ƚ��
               05  RECE841-KNSJISKBN           PIC  9(01).
      *�������»�Ƚ�ǥ��롼��
               05  RECE841-KNSJISGRPKBN        PIC  9(02).
      *���ǡ����̱���
               05  RECE841-FLG-OSIN-COMMENT    PIC  X(01).
      *�߻ٱ�˴ؤ������
               05  RECE841-FLG-ZAISIEN-COMMENT PIC  X(01).
      *�쥻�����������û��ե饰
               05  RECE841-NYUKSNFLG           PIC  9(01).
      *�����ʣ£ΣС��Σԡݣ���£ΣС��ȣ��ΣС�
               05  RECE841-FLG-KENSA-COMMENT   PIC  X(01).
      *
      *���ٱ�Ϣ�Ȼ�Ƴ��
               05  RECE841-FLG-KAISIEN-COMMENT PIC  X(01).
      *���ı��ܿ�����Ƴ��
               05  RECE841-FLG-SYUEIYO-COMMENT PIC  X(01).
      *�͹���¡��
               05  RECE841-FLG-JINZOU-COMMENT  PIC  X(01).
      *�ɽ걢���ĺ�����
               05  RECE841-FLG-KYOKUIN-COMMENT PIC  X(01).
      *���
               05  RECE841-FLG-MASUI-COMMENT   PIC  X(01).
      *���ܥ��ݡ��ȥ�����û�
               05  RECE841-FLG-EISAPO-COMMENT  PIC  X(01).
      *�Ƶۥ���������û�
               05  RECE841-FLG-KOKKEA-COMMENT  PIC  X(01).
      *�žɼԲû�����������������������
               05  RECE841-FLG-JYUSYO-COMMENT  PIC  X(01).
      *
      *�������ǥե����
               05  RECE841-FLG-FILM-COMMENT    PIC  X(01).
      *
      *������ӽ��۰����ؼ���
               05  RECE841-FLG-KAISYOKU-COMMENT
                                               PIC  X(01).
      *�����ʥ��硼�ȡ������������ʥǥ�������
               05  RECE841-FLG-SEIKEA-COMMENT  PIC  X(01).
      *�����ʥꥨ���������û�
               05  RECE841-FLG-SEIRIE-COMMENT  PIC  X(01).
      *�������޶�̳�»ܲû�
               05  RECE841-FLG-BYOYAK-COMMENT  PIC  X(01).
      *
      *�����³�۰��Ƶ���ˡ��Ƴ������
               05  RECE841-FLG-YOUATU-COMMENT  PIC  X(01).
      *
      *��¡�ڡ����᡼������Ƴ�����������ѷ���ư����ư���
      *��¡�ڡ����᡼������Ƴ�������ʥ�������������ư��ܹԴ��û��ޤ��
               05  RECE841-FLG-SINPESUR-COMMENT  PIC  X(01).
      *��֥�˥���󥰲û�
               05  RECE841-FLG-SINPESU-COMMENT   PIC  X(01).
      *
      *���󴵼Ի�Ƴ������������
               05  RECE841-FLG-GANSIDO-COMMENT   PIC  X(01).
      *���𴵼Զ�Ʊ������
               05  RECE841-FLG-ZAIKYO-COMMENT    PIC  X(01).
      *�����ˤ��������ɷ�ˡ
               05  RECE841-FLG-BOKOHO-COMMENT    PIC  X(01).
      *Ķ�žɻ��ʼԡ��������Ųû�����Ķ�žɻ��ʼԡ��������Ųû�
               05  RECE841-FLG-CYOJKSN-COMMENT   PIC  X(01).
      *��Ǣ��Ω��Ƴ��
               05  RECE841-FLG-HAIJRT-COMMENT    PIC  X(01).
      *����Ⱦ�Ƿ����ܷдɱ���ˡ��Ƴ������
               05  RECE841-FLG-ZAIHANKO-COMMENT  PIC  X(01).
      *�ⵤ�����Ǽ���
               05  RECE841-FLG-KIATU-COMMENT     PIC  X(01).
      *������������ץ������û�
               05  RECE841-FLG-SCPRO-COMMENT     PIC  X(01).
      *
      *���ֳ��۵ޱ��⸡���û��������®���θ����û�
               05  RECE841-FLG-KENKSN-COMMENT    PIC  X(01).
      *���ֳ��۵ޱ���������ǲû�
               05  RECE841-FLG-KINGAZ-COMMENT    PIC  X(01).
      *
      *�ౡ����Ʊ��Ƴ����
               05  RECE841-FLG-TAIKYO-COMMENT    PIC  X(01).
      *
      *�͹��⼪����Ĵ���û�
               05  RECE841-FLG-JINNAI-COMMENT    PIC  X(01).
      *
      *��Ǣ��Ω�ٱ�û�
               05  RECE841-FLG-HAIJSK-COMMENT    PIC  X(01).
      *
      *�굻������ơʼ�ѡ��������ǡ����֡�
               05  RECE841-ACCTP-SYUGI-TBL-G.
                   07  RECE841-ACCTP-SYUGI-TBL     OCCURS  5.
      *�굻���ϥ�����
                       09  RECE841-ACCTP-SRYCD     PIC X(09).
      *�굻����
                       09  RECE841-ACCTP-SYUTEN    PIC 9(07).
      *��������
                       09  RECE841-ACCTP-YKZTEN    PIC 9(07).
      *����¾��������ʥե���ࡦ���ǡ����ǰʳ���
                       09  RECE841-ACCTP-KIZAITEN  PIC 9(07).
      *�ե��������
                       09  RECE841-ACCTP-FIRTEN    PIC 9(07).
      *��������
               05  RECE841-ACCTP-SANSOTEN          PIC 9(07).
      *��������
               05  RECE841-ACCTP-CHISOTEN          PIC 9(07).
      *
      *������ʬ��������
               05  RECE841-HOKATU-TENSU-AREA.
      *������
                   07  RECE841-HOKATU-ZAITEN       PIC 9(08).
      *�굻������
                   07  RECE841-HOKATU-SYUTEN1      PIC 9(07).
      *�굻������
                   07  RECE841-HOKATU-SYUTEN2      PIC 9(07).
      *��������
                   07  RECE841-HOKATU-YKZTEN       PIC 9(07).
      *�������
                   07  RECE841-HOKATU-KIZAITEN     PIC 9(07).
      *
      *�ޡ�������
               05  RECE841-ZAI-TENSU               PIC 9(09).
      *�ޡʶ�ۡ�
               05  RECE841-ZAI-KINGAKU             PIC 9(09).
      *
      *��������ơ��֥�
               05  RECE841-DAY-AREA.
      *        ���Ų���ơ��֥��1:���,2:����,3:����, 4:����ʾ��
                   07  RECE841-DAY-TBL             OCCURS  4.
                       09  RECE841-KAISUINF        OCCURS  31.
                           11  RECE841-KAISU       PIC 9(03).
      *�쥻�ץȼ���
               05  RECE84-RECESYUBETU          PIC 9(04).