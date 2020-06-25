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
      *    �����쥻�ץȡ������ˡ���������    COPY (CPRCF220.INC) 
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.03.01    NACL-����    08/07/14  �����������ե饰���ɲ�
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
      *  04.07.02    NACL-����    14/04/07  �����б������ɲ�
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
      *01  RECE220-REC.
           03  RECE220-KEY.
               05  RECE220-KEY1.
      *���ŵ����ֹ�
                   07  RECE220-HOSPNUM     PIC 9(2).
      *����ǯ��
                   07  RECE220-SRYYM       PIC 9(06).
      *������ʬ
                   07  RECE220-NYUGAIKBN   PIC X(01).
      *���ԣɣ�
                   07  RECE220-PTID        PIC 9(10).
      *�쥻�ץȿ��Ų�
                   07  RECE220-RECEKA      PIC X(02).
      *����ɣ�
                   07  RECE220-HKNID       PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE220-TEKSTYMD    PIC X(08). 
      *���ֹ�
               05  RECE220-ZAINUM          PIC 9(08).
      *���Ŷ�ʬ
           03  RECE220-SRYKBN              PIC X(02).
      *�޼��̶�ʬ
           03  RECE220-ZAISKBKBN           PIC X(01).
      *�����ֹ�
           03  RECE220-PTNUM               PIC X(20).
      *���Ų�
           03  RECE220-SRYKA               PIC X(02).
      *�ݸ��ȹ礻�ֹ�
           03  RECE220-HKNCOMBI            PIC 9(04).
      *���ݶ�ʬ
           03  RECE220-JIHOKBN             PIC X(01).
      *������
           03  RECE220-ZAITEN              PIC 9(08).
      *���ť����ɷ�
           03  RECE220-SRYCDTOTAL          PIC 9(14).
      *���̷�
           03  RECE220-SURYOUTOTAL         PIC 9(07)V9(05).
      *���ť����ɡ����̤��߷�
           03  RECE220-RUIKEITOTAL         PIC 9(13)V9(05).
      *���ٿ�
           03  RECE220-MEISAISU            PIC 9(04).
      *�޲��
           03  RECE220-ZAIKAISU            PIC 9(04).
      *�굻������
           03  RECE220-SYUTEN1             PIC 9(07).
      *�굻������
           03  RECE220-SYUTEN2             PIC 9(07).
      *��������
           03  RECE220-YKZTEN              PIC 9(07).
      *�������
           03  RECE220-KIZAITEN            PIC 9(07).
      *�ɥ�����������
           03  RECE220-DRCD                PIC X(05).
      *������ե饰
           03  RECE220-ZAIREQFLG           PIC X(01).
      *����Ψ
           03  RECE220-TEIGENRATE          PIC X(01).
      *�����ȥ����ɥե饰
           03  RECE220-COMCDFLG            PIC X(01).
      *�ʥ��˥ե饰
           03  RECE220-SAFLG               PIC X(01).
      *�����������ʣ����ϣ��ʥ����ȤΤߡˡ�
           03  RECE220-TOYAKUFLG           PIC X(01).
      *�������������������������ٿ�
           03  RECE220-RYOTENKISAISU       PIC 9(04).
      *���򼰥�����¸�ߥե饰
           03  RECE220-SENTAKUCOMFLG       PIC X(01).
      *�޶�ʬ
           03  RECE220-ZAIKBN              PIC 9(03).
      *�������ǡʣ����ܰʹ߸��������
           03  RECE220-GAZOGEN             PIC X(01).
      *�ƿǲû��������ˤκ�
           03  RECE220-NYUSAIFLG           PIC X(01).
      *�����߰ʲ����ޥ쥻�ץȵ��ܥե饰
           03  RECE220-YKZKISAIFLG         PIC X(01).
      *��������ʬ
           03  RECE220-JISANYAKKBN         PIC X(01).
      *
      *��ϥӥ�
           03  RECE220-RIHABIRI-COMMENT    PIC X(01).
      *��ϥӥ�ʿ����ɼ�����
           03  RECE220-SINDAI-COMMENT      PIC X(01).
      *��ϥӥ�ʸƵ۴��
           03  RECE220-KOKYUKI-COMMENT     PIC X(01).
      *��ϥӥ��Ǿ��ɼ�������
           03  RECE220-NOUKE-COMMENT       PIC X(01).
      *��ϥӥ�ʱ�ư���
           03  RECE220-UNDOKI-COMMENT      PIC X(01).
      *��ϥӥ�����´��ԡ�
           03  RECE220-NANBYO-COMMENT      PIC X(01).
      *��ϥӥ�ʾ㳲���ʼԡˡ�
           03  RECE220-SYOGAI-COMMENT      PIC X(01).
      *��ϥӥ���ݿ���
           03  RECE220-SESSYOKU-COMMENT    PIC X(01).
      *��ϥӥ�����Ѿɸ�����
           03  RECE220-HAIYO-COMMENT       PIC X(01).
      *��ã����
           03  RECE220-KAITATU-COMMENT     PIC X(01).
      *
      *��ϥӥ�û��ʿ����ɼ�����
           03  RECE220-SINDAI9-COMMENT     PIC X(01).
      *��ϥӥ�û��ʸƵ۴��
           03  RECE220-KOKYUKI9-COMMENT    PIC X(01).
      *��ϥӥ�û������Ѿɸ�����
           03  RECE220-HAIYO9-COMMENT      PIC X(01).
      *
      *���޴�����Ƴ��
           03  RECE220-FLG-YAKUKAN-COMMENT PIC X(01).
      *�������ܿ�����Ƴ��
           03  RECE220-FLG-NYUEIYO-COMMENT PIC X(01).
      *�ౡ��ˬ���Ƴ��
           03  RECE220-FLG-HOUMON-COMMENT  PIC X(01).
      *���¼�����ո��������
           03  RECE220-FLG-SYOBYO-COMMENT  PIC X(01).
      *������Ʊ�ս������
           03  RECE220-FLG-RYOYO-COMMENT   PIC X(01).
      *�ౡ��������Ƴ�û�����������޴�����Ƴ��
           03  RECE220-FLG-TAIIN-COMMENT   PIC X(01).
      *ˬ��Ǹ�ؼ���
           03  RECE220-FLG-HOUSIJI-COMMENT PIC X(01).
      *��ѡ�͢��Ͻ�����
           03  RECE220-FLG-SHUJUTU-COMMENT PIC X(01).
      *�ãԡ��ͣң�
           03  RECE220-FLG-CTMRI-COMMENT   PIC X(01).
      *�������ౡ��ˬ���Ƴ��
           03  RECE220-FLG-HOUSEI-COMMENT  PIC X(01).
      *�����ʳ�Υ�¼������û����������û���
           03  RECE220-FLG-KAKURI-COMMENT  PIC X(01).
      *������Ʃ���Ѳ��Ƴ������
           03  RECE220-FLG-HINKAI-COMMENT  PIC X(01).
      *����
           03  RECE220-FLG-SANSO-COMMENT   PIC X(01).
      *��ǡ�Ʊ��ʣ�����Ųʡ�
           03  RECE220-FLG-SYODOU-COMMENT  PIC X(01).
           03  RECE220-SYODOU-SRYKA-AREA.
               05  RECE220-SYODOU-SRYKA    PIC X(02)   OCCURS  10.
      *�������»�Ƚ��
           03  RECE220-KNSJISKBN           PIC 9(01).
      *�������»�Ƚ�ǥ��롼��
           03  RECE220-KNSJISGRPKBN        PIC 9(02).
      *���ǡ����̱���
           03  RECE220-FLG-OSIN-COMMENT    PIC X(01).
      *�߻ٱ�˴ؤ������
           03  RECE220-FLG-ZAISIEN-COMMENT PIC X(01).
      *�����ʣ£ΣС��Σԡݣ���£ΣС��ȣ��ΣС�
           03  RECE220-FLG-KENSA-COMMENT   PIC X(01).
      *
      *���ٱ�Ϣ�Ȼ�Ƴ��
           03  RECE220-FLG-KAISIEN-COMMENT PIC X(01).
      *���ı��ܿ�����Ƴ��
           03  RECE220-FLG-SYUEIYO-COMMENT PIC X(01).
      *�͹���¡��
           03  RECE220-FLG-JINZOU-COMMENT  PIC X(01).
      *�ɽ걢���ĺ�����
           03  RECE220-FLG-KYOKUIN-COMMENT PIC X(01).
      *���
           03  RECE220-FLG-MASUI-COMMENT   PIC X(01).
      *���ܥ��ݡ��ȥ�����û�
           03  RECE220-FLG-EISAPO-COMMENT  PIC X(01).
      *�Ƶۥ���������û�
           03  RECE220-FLG-KOKKEA-COMMENT  PIC X(01).
      *�žɼԲû�����������������������
           03  RECE220-FLG-JYUSYO-COMMENT  PIC X(01).
      *
      *������ӽ��۰����ؼ���
           03  RECE220-FLG-KAISYOKU-COMMENT
                                           PIC  X(01).
      *�����ʥ��硼�ȡ������������ʥǥ�������
           03  RECE220-FLG-SEIKEA-COMMENT  PIC  X(01).
      *�����ʥꥨ���������û�
           03  RECE220-FLG-SEIRIE-COMMENT  PIC  X(01).
      *�������޶�̳�»ܲû�
           03  RECE220-FLG-BYOYAK-COMMENT  PIC  X(01).
      *
      *�����³�۰��Ƶ���ˡ��Ƴ������
           03  RECE220-FLG-YOUATU-COMMENT  PIC  X(01).
      *
      *��¡�ڡ����᡼������Ƴ�����������ѷ���ư����ư���
      *��¡�ڡ����᡼������Ƴ�������ʥ�������������ư��ܹԴ��û��ޤ��
           03  RECE220-FLG-SINPESUR-COMMENT  PIC  X(01).
      *��֥�˥���󥰲û�
           03  RECE220-FLG-SINPESU-COMMENT   PIC  X(01).
      *
      *���󴵼Ի�Ƴ������������
           03  RECE220-FLG-GANSIDO-COMMENT   PIC  X(01).
      *���𴵼Զ�Ʊ������
           03  RECE220-FLG-ZAIKYO-COMMENT    PIC  X(01).
      *�����ˤ��������ɷ�ˡ
           03  RECE220-FLG-BOKOHO-COMMENT    PIC  X(01).
      *Ķ�žɻ��ʼԡ��������Ųû�����Ķ�žɻ��ʼԡ��������Ųû�
           03  RECE220-FLG-CYOJKSN-COMMENT   PIC  X(01).
      *��Ǣ��Ω��Ƴ��
           03  RECE220-FLG-HAIJRT-COMMENT    PIC  X(01).
      *����Ⱦ�Ƿ����ܷдɱ���ˡ��Ƴ������
           03  RECE220-FLG-ZAIHANKO-COMMENT  PIC  X(01).
      *�ⵤ�����Ǽ���
           03  RECE220-FLG-KIATU-COMMENT     PIC  X(01).
      *������������ץ������û�
           03  RECE220-FLG-SCPRO-COMMENT     PIC  X(01).
      *
      *���ֳ��۵ޱ��⸡���û��������®���θ����û�
           03  RECE220-FLG-KENKSN-COMMENT    PIC  X(01).
      *���ֳ��۵ޱ���������ǲû�
           03  RECE220-FLG-KINGAZ-COMMENT    PIC  X(01).
      *
      *�ౡ����Ʊ��Ƴ����
           03  RECE220-FLG-TAIKYO-COMMENT    PIC  X(01).
      *
      *�͹��⼪����Ĵ���û�
           03  RECE220-FLG-JINNAI-COMMENT    PIC  X(01).
      *
      *��Ǣ��Ω�ٱ�û�
           03  RECE220-FLG-HAIJSK-COMMENT    PIC  X(01).
      *
      *��������ơ��֥�
           03  RECE220-DAY-AREA.
      *        ���Ų���ơ��֥��1:���,2:����,3:����, 4:����ʾ��
             04  RECE220-DAY-TBL           OCCURS  4.
                 05  RECE220-KAISUINF      OCCURS  31.
                     07  RECE220-KAISU     PIC 9(03).
      *
      *������ʬ��������
           03  RECE220-HOKATU-TENSU-AREA.
      *������
               05  RECE220-HOKATU-ZAITEN   PIC  9(08).
      *�굻������
               05  RECE220-HOKATU-SYUTEN1  PIC  9(07).
      *�굻������
               05  RECE220-HOKATU-SYUTEN2  PIC  9(07).
      *��������
               05  RECE220-HOKATU-YKZTEN   PIC  9(07).
      *�������
               05  RECE220-HOKATU-KIZAITEN PIC  9(07).