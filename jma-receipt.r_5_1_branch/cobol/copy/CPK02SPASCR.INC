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
      *****************************************************************
      *    ���Ź԰٣ӣУ����ǡ��������� ���� COPY (CPK02SPASCR.INC)
      *             ���̤Υ����ΰ��ǡ����ǻ��Ѥ���
      *                                  �ʳƿ��Ź԰٥����å��˻��ѡ�
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     MCC-¿��Ǽ   02/03/05  �������ѿ����ɲ�
      * 01.00.02     MCC-¿��Ǽ   02/03/15  ��ȯ���������ɲ�
      * 01.00.03     MCC-¿��Ǽ   02/04/09  ϫ�����ɲ�(RL=487��200+4)
      * 01.00.04     MCC-¿��Ǽ   02/05/27  �ش����ɲ�(RL=488��200+4)
      * 01.00.05     MCC-¿��Ǽ   02/05/27  �ش����ɲ�(RL=510��200+4)
      * 01.00.06     NACL-¿��Ǽ  02/10/23  �����б��ɲ�(RL=674��200+4)
      * 01.00.07     NACL-¿��Ǽ  03/04/30  �������ɲ�(RL=682��200+4)
      * 01.00.08     NACL-¿��Ǽ  03/05/12  ̾�Τ򣸣���
      * 01.00.09     NACL-¿��Ǽ  05/04/01  �𼨶�ʬ�ɲ�
      * 01.00.10     NACL-¿��Ǽ  05/06/01  ���θ������������ɲ�
      * 01.00.10     NACL-¿��Ǽ  05/06/10  �����ʬ���ɲ�
      * 01.00.11     NACL-¿��Ǽ  06/03/17  �������Ļ��û��ɲ�
      * 03.04.00     NACL-¿��Ǽ  06/12/11  �������þ����ɲ�
      * 04.00.00     NACL-¿��Ǽ  07/09/XX  �ơ��֥룴������
      * 04.02.00     NACL-¿��Ǽ  08/03/XX  ��������ѻٱ�
      *                                     ��켱���ɲ�
      * 04.05.00     NACL-¿��Ǽ  09/10/02  ����������¾�б�
      * 04.06.00     NACL-¿��Ǽ  11/02/14  ��ˡ�������б�
      * 04.07.00     NACL-¿��Ǽ  11/09/21  �������������б�
      * 04.07.00     NACL-¿��Ǽ  11/12/20  ����̾������ñ���б�
      * 04.07.00     NACL-¿��Ǽ  12/11/07  �ٹ��ʬ���ɲ�
      * 04.08.00     NACL-¿��Ǽ  16/02/XX  �ȣ���ǯ�����б�
      * 05.00.00     NACL-¿��Ǽ  20/03/XX  �ң���ǯ�����б�
      ************************************************************
      *
      *01  SPA-K02SCR-REC.
      *
           03  SPA-GMN-MAX             PIC 9(04).
      *
           03  SPA-GMN-REC.
               05  SPA-GMN-TBL             OCCURS   400.
                   07  SPA-GMN-TBLREC.
                       09  SPA-GMN-SRYKBN        PIC X(02).
                       09  SPA-GMN-INPUTCD       PIC X(54).
                       09  SPA-GMN-MEISYO-G.
                           11  SPA-GMN-MEIH      PIC X(02).
      *                    11  SPA-GMN-MEISYO    PIC X(78).
      *R02.3
                           11  SPA-GMN-MEISYO    PIC X(138).
      **               09  SPA-GMN-SURYO         PIC X(09).
                       09  SPA-GMN-SURYO         PIC X(11).
                       09  SPA-GMN-TANINAME      PIC X(12).
                       09  SPA-GMN-TENSU         PIC X(08).
                       09  SPA-GMN-KAISU         PIC X(04).
                       09  SPA-GMN-KEI           PIC S9(08).
      *---(01.00.01)
      *                ���ϥ����ɤη��
                       09  SPA-GMN-INCNT         PIC 9(02).
      *
                   07  SPA-COM-TBLREC.
      *---(01.00.01)
                       09  SPA-COM-SET           PIC X(01).
                       09  SPA-COM-PAGE          PIC 9(04).
                       09  SPA-COM-GYOU          PIC 9(04).
                       09  SPA-COM-CUR           PIC X(01).
      *                �������ϰ���
                       09  SPA-COM-IN            PIC X(01).
      *                ��������ͭ��
                       09  SPA-COM-IN2           PIC X(01).
      *                ��������ͭ��
                       09  SPA-COM-IN3           PIC X(01).
      *ver.4.7         ����ͭ��
                       09  SPA-COM-IN4           PIC X(01).
                       09  SPA-MAE-INPUTCD       PIC X(54).
      *---(01.00.01)
      *
                       09  SPA-COM-SRYSYUKBN     PIC X(03).
                       09  SPA-COM-SRYKBN        PIC X(02).
                       09  SPA-COM-INPUTCD       PIC X(09).
                       09  SPA-COM-SURYO         PIC 9(05)V9(05).
      *                �޹������
                       09  SPA-COM-TENSU         PIC 9(08).
      *                �ޥ��ʥ���ʬ
                       09  SPA-COM-PLUSKBN       PIC 9(01).
      *                �������ٶ��
                       09  SPA-COM-JIHIMONEY     PIC 9(07).
      *                ����
      *R02.4           09  SPA-COM-TEN           PIC 9(08)V9(4).
                       09  SPA-COM-TEN           PIC 9(10)V9(4).
      *                �����ʤ������ͷ׻����
      *R02.4           09  SPA-COM-KISOTEN       PIC 9(08)V9(4).
                       09  SPA-COM-KISOTEN       PIC 9(10)V9(4).
                       09  SPA-COM-KAISU         PIC 9(03).
                       09  SPA-COM-KEI           PIC 9(08).
      *                ��������������
                       09  SPA-COM-SURYO2        PIC 9(05)V9(05).
                       09  SPA-COM-KAISU2        PIC 9(03).
      *
                       09  SPA-COM-BUNGSU        PIC 9(08).
                       09  SPA-COM-INPUTCHI-G.
                           11  SPA-COM-ATAI1         PIC X(10).
                           11  SPA-COM-ATAI2         PIC X(10).
                           11  SPA-COM-ATAI3         PIC X(10).
                           11  SPA-COM-ATAI4         PIC X(10).
                           11  SPA-COM-ATAI5         PIC X(10).
                       09  SPA-COM-INPUTCHI-R    REDEFINES
                                                 SPA-COM-INPUTCHI-G.
                           11  SPA-COM-ATAI          PIC X(10)
                                                     OCCURS  5.
      *R02.4
      *                8311 �����ϣã�
                       09  SPA-GMN-ATAI83        PIC X(20).
      *
      *                �굻���ף�
                       09  SPA-COM-SYUTEN1       PIC 9(08).
      *                �굻���ף�
                       09  SPA-COM-SYUTEN2       PIC 9(08).
      *                ��������
                       09  SPA-COM-YKZTEN        PIC 9(08).
      *                �������
                       09  SPA-COM-KIZAITEN      PIC 9(08).
      *
                       09  SPA-COM-ZAINUM        PIC 9(08).
      *                �޽�λ
                       09  SPA-COM-ZAIEND        PIC X(01).
      *                ��ư�����ʬ�ʣ�������ɽ�����������Ͼ��ˤ���ѹ���
                       09  SPA-COM-AUTOKBN       PIC X(01).
                       09  SPA-COM-AUTOKBN2      PIC X(01).
      *                ��������
                       09  SPA-COM-SURFLG        PIC X(01).
      *                �������
                       09  SPA-COM-KAIFLG        PIC X(01).
      *                ̾������
                       09  SPA-COM-MEIFLG        PIC X(01).
      *                ������
                       09  SPA-COM-KEIFLG        PIC X(01).
      *                ��������
                       09  SPA-COM-JIHIFLG       PIC X(01).
      *
      *                �������ѿ��Ź԰٥�����
                       09  SPA-COM-CHK-SRYCD     PIC X(09).
      *                ����ɽ�������ϥ�����
                       09  SPA-COM-ICD-INPUTCD   PIC X(22).
      *                ���ϥ�����ɽ���Ѥ�
                       09  SPA-COM-GMNFLG        PIC X(01).
      *
                       09  SPA-COM-FLGAREA.
      *                    �ش����޷ٹ�ѥե饰
                           11  SPA-COM-KNKFLG        PIC X(01).
      *                    �ش����ޥ����å��ѥե饰
                           11  SPA-COM-KNKUMU        PIC X(01).
      *                    ϫ���ɤ��ؤ��û�ʬ
                           11  SPA-COM-RSIFLG2       PIC X(01).
      *                    ���̥����å��ѡ���Ϳ�̥����å���
                           11  SPA-COM-SURCHK        PIC X(01).
      *                    ʻ�ѻ���ѥե饰
                           11  SPA-COM-HCHKFLG       PIC X(01).
      *                    ʻ�ѻ���ٹ�ѥե饰
                           11  SPA-COM-HKEIFLG       PIC X(01).
      *                    ���ܥ����å��ѥե饰
                           11  SPA-COM-CHKFLG        PIC X(01).
      *                    ���ֲû��ե饰
                           11  SPA-COM-TIMEFLG       PIC 9(01).
      *ver.4.8             �ƿǼ�ư����ե饰
                           11  SPA-COM-KSNAUTO       PIC X(01).
      *                �ٹ�Ѥߥե饰
                       09  SPA-COM-KZMFLGAREA.
      *                    ���Զش����ޥ����å��ѥե饰
                           11  SPA-COM-KINKIFLG      PIC X(01).
      *                    �ٹ�ѥե饰
                           11  SPA-COM-KZMFLG        PIC X(01).
      *                    �ٹ�ѥե饰��
                           11  SPA-COM-KZMFLG2       PIC X(01).
      *                    �ٹ�ѥե饰����Ʊ��ʻ�ѥ����å���
                           11  SPA-COM-KZMFLG3       PIC X(01).
      *                    �ٹ�ѥե饰����Ĺ�����������å���
                           11  SPA-COM-KZMFLG4       PIC X(01).
      *                    �ٹ�ѥե饰������Ϳ�̥����å���
                           11  SPA-COM-KZMFLG5       PIC X(01).
      *ver.4.7             �����������ٹ�
                           11  SPA-COM-KZMFLG6       PIC X(01).
      *                    �ٹ�ʼ������ٹ��
                           11  SPA-COM-KZMFLG7       PIC X(01).
      *                    �ٹ�ʥ�ϥӥ긺����
                           11  SPA-COM-KZMFLG8       PIC X(01).
      *                    �ٹ����绻�����Ϸٹ�(H29.9��
                           11  SPA-COM-KZMFLG9       PIC X(01).
      *                    �ٹ�ʥ�ϲ���(H31.4��
                           11  SPA-COM-KZMFLG10      PIC X(01).
      *                    ������ʻ�ѷٹ�
                           11  SPA-COM-KZMFLGNK      PIC X(01).
      *                    ����������ʤ��ٹ�
                           11  SPA-COM-KZMFLGN2      PIC X(01).
      *
      *                ��ưȯ���ѥե饰
                       09  SPA-COM-AUTOFLG           PIC X(01).
      *                �����Ѵ���ưȯ���ѥե饰
                       09  SPA-COM-AUTOFLG2          PIC X(01).
      *                �׻������å��ѥե饰����󥯥ꥢ��
                       09  SPA-COM-SANFLG        PIC 9(01).
      *                ���������о�̵ͭ�ʣ�������ͭ���
                       09  SPA-COM-TEIGEN-FLG    PIC  9(01).
      *                ���������оݥ����å���
                       09  SPA-COM-TEIGEN-CHK    PIC  9(01).
      *                ϫ�Ҷ�ʬ��ϫ���ѤΥ����ɡ�
                       09  SPA-COM-RSIKBN        PIC  9(01).
      *                ϫ�� �ʳ�������û����ɤ��ؤ��ե饰��
                       09  SPA-COM-RSIGIRI       PIC  9(01).
      *                �������������
                       09  SPA-COM-NYUIN-AREA.
                           11  SPA-COM-NYU-KAI        PIC 9(03).
                           11  SPA-COM-NYUINDAY       PIC 9(01)
                                                      OCCURS  31.
      *                �����ٹ�ѥե饰�����͡�
                       09  SPA-COM-NYU-KZMFLG-G.
                           11  SPA-COM-NYU-KZMFLG        PIC X(01)
                                                     OCCURS   31.
      *                �ݸ��ȹ礻�ʶ���������ݸ���
                       09  SPA-COM-HKNCOMBI      PIC  X(04).
      *                �����ݸ��ʣ���������
                       09  SPA-COM-CHIKENKBN     PIC  X(01).
      *                ���Ųʡʶ���������
                       09  SPA-COM-SRYKA         PIC  X(02).
      *                �ɥ������ʶ���������
                       09  SPA-COM-DRCD          PIC  X(04).
      *                �޹����������������
                       09  SPA-COM-MAE-TENSU     PIC 9(08).
      *                ��������ʬ
                       09  SPA-COM-MAE-AUTOKBN   PIC X(01).
      *                Ʊ�측��Ƚ�ꥳ����
                       09  SPA-COM-KENGRP        PIC  X(09).
      *                ��ȯ������̵ͭ�ʣ�����ȯ�����
                       09  SPA-COM-KOUHATU-FLG   PIC  9(01).
      *                ����Υ����å��ǡ�������
                       09  SPA-COM-DAYCHK-FLG    PIC  9(01).
      *!????
      *                ��祳���ɡʥ����ƥ�������
                       09  SPA-COM-HOUKATU-KFLG  PIC  9(01).
                       09  SPA-COM-HOUKATU-KFLG2 PIC  9(01).
                       09  SPA-COM-HOUKATU-KERR  PIC  9(01).
      *                ��祳���ɡʣ������ʬ��
                       09  SPA-COM-HOUKATU-FLG   PIC  9(01).
                       09  SPA-COM-HOUKATU-FLG2  PIC  9(01).
      *                ����
                       09  SPA-COM-HOUKATU-ZAI   PIC  9(01).
      *                ��票��ݥǡ���
                       09  SPA-COM-HOUKATU-ERR   PIC  9(01).
      *ver.4.3
      *                ���θ����������ܡ��������û��ѡ�
                       09  SPA-COM-HOUKNS-10     PIC  9(01).
      *ver.4.5.0
      *                ������������
                       09  SPA-COM-KANSURYO       PIC 9(05)V9(05).
      *                �����������ʬ
                       09  SPA-COM-TANDKKBN       PIC X(01).
      *                ������ؼ�����
                       09  SPA-COM-INPUTNAIF      PIC X(01).
      *                ��³����������
                       09  SPA-COM-INPUTCONT      PIC X(01).
      *                ��ɾ����
                       09  SPA-COM-YAKUHYO        PIC X(01).
      *                ��������
                       09  SPA-COM-GENTEN         PIC X(01).
      *
      *                ���¿��̥����å�
                       09  SPA-COM-TUKIJGNFLG     PIC X(01).
                       09  SPA-COM-TUKIJGNFLG1    PIC X(01).
                       09  SPA-COM-TUKIJGNFLG2    PIC 9(01).
                       09  SPA-COM-KAIFLG2        PIC X(01).
      *ver.4.5.0
      *
      *        ���Ų�����ù���
                       09  SPA-COM-PLUS-AREA.
      *                �굻��
                           11  SPA-COM-SYUFLG        PIC  9(01).
      *                ϫ�Ҷ��
                           11  SPA-COM-RSIKIN        PIC  9(08).
      *                ��������
                           11  SPA-COM-SANSOTEN      PIC  9(08).
      *                ��������
                           11  SPA-COM-CHISOTEN      PIC  9(08).
      *                �ե��������
                           11  SPA-COM-FIRTEN-M      PIC  9(08).
      *                �굻�������
                           11  SPA-COM-SYUTEN-M      PIC  9(08).
      *                ��������
                           11  SPA-COM-YKZTEN-M      PIC 9(08).
      *                �������
                           11  SPA-COM-KIZTEN-M      PIC 9(08).
      *ver.4.7
      *                ��������������
                           11  SPA-COM-INGAITEN      PIC 9(08).
      *
      *        �����ޥ�������
      *                �����ޥ����ο��Ŷ�ʬ
                       09  SPA-COM-TNSSRYKBN     PIC X(02).
      *                �����ޥ����ο��ż���
                       09  SPA-COM-TNSSRYSYUKBN  PIC X(03).
      *                �ǡ�����ʬ
                       09  SPA-COM-DATAKBN       PIC X(01).
      *                �����ޥ���̾�Ρ���縡���ѡ�
      *R02.4           09  SPA-COM-NAME          PIC X(78).
                       09  SPA-COM-NAME          PIC X(100).
      *                �����ޥ�������������
                       09  SPA-COM-MADOKUKBN     PIC 9(01).
                       09  SPA-COM-SINKEIKBN     PIC 9(01).
                       09  SPA-COM-SEIBUTUKBN    PIC 9(01).
                       09  SPA-COM-ZOEIZAIKBN    PIC 9(01).
                       09  SPA-COM-CSYYOURYO     PIC 9(05).
      *                ñ�̥�����
                       09  SPA-COM-TANICD        PIC X(03).
                       09  SPA-COM-TANINAME      PIC X(12).
      *                ���޶�ʬ
                       09  SPA-COM-YKZKBN        PIC 9(01).
                       09  SPA-COM-ZAIGATAKBN    PIC 9(01).
      *                �����ޥ��������̶�ʬ
                       09  SPA-COM-BUIKBN        PIC X(01).
      *                �������»�Ƚ�Ƕ�ʬ
                       09  SPA-COM-KNSJISKBN     PIC  9(01).
      *                �������»�Ƚ�ǥ��롼��
                       09  SPA-COM-KNSJISGRPKBN  PIC  9(02).
      *                ���û�������
                       09  SPA-COM-CHUKSNCD      PIC  X(04).
      *                ���û�����
                       09  SPA-COM-CHUKSNTUBAN   PIC  X(01).
      *                ��§�û��оݳ���ʬ
                       09  SPA-COM-TUSOKUGAIKBN   PIC  X(01).
      *                ��§ǯ���ʬ
                       09  SPA-COM-TSUSOKUAGE    PIC  X(01).
      *                ���ֲû���ʬ
                       09  SPA-COM-TIMEKSNKBN    PIC  9(01).
      *                �����ѡ�����оݸ���
                       09  SPA-COM-HOUKNSKBN     PIC  9(02).
      *                �����ѡ����θ���������
                       09  SPA-COM-KENTAICOMMENT     PIC  9(01).
      *                �����ѡ���ʬ�ֹ�
                       09  SPA-COM-CDKBN.
                           11  SPA-COM-CDKBN-KBN   PIC  X(01).
      ****                 11  SPA-COM-CDKBN-SYO   PIC  X(01).
      ****                 11  SPA-COM-CDKBN-BU    PIC  X(02).
      *                    ϫ���ѡ���ʬ�ֹ�
                           11  SPA-COM-CDKBN-KBNNUM     PIC  X(03).
      *                    ϫ���ѡ���ʬ�ֹ����
                           11  SPA-COM-CDKBN-KBNNUM-EDA PIC  X(02).
      *H28.9               ����
                           11  SPA-COM-CDKBN-KOUBAN     PIC  X(02).
      *                ���������ʬ
                       09  SPA-COM-SANTEIRRKKBN    PIC  9(01).
      *                ��������û���ʬ
                       09  SPA-COM-GAIKANRIKBN     PIC  9(01).
      *                ��Ƴ������
                       09  SPA-COM-SDOKNRYO        PIC  9(02).
      *                ��������
                       09  SPA-COM-TENSIKIBETU     PIC  9(01).
      *                ������ǯ��û���ʬ
                       09  SPA-COM-TOKUKIZAIAGEKSNKBN   PIC  9(01).
      *                ��������ʬ
                       09  SPA-COM-SANSOKBN        PIC  9(01).
      *                ��������̣�
                       09  SPA-COM-TOKUKIZAISBT1   PIC  9(01).
      *                ��������ʴ��ξ�¡�
                       09  SPA-COM-JGNTEN          PIC  9(07).
      *                �ݸ�Ŭ�Ѷ�ʬ
                       09  SPA-COM-HKNTEKKBN       PIC  9(01).
      *                �����󽸷��輱��
                       09  SPA-COM-GAITENTTLKBN    PIC  9(03).
      *                �����󽸷��輱��
                       09  SPA-COM-NYUTENTTLKBN    PIC  9(03).
      *                ���Ƕ�ʬ
                       09  SPA-COM-OSINKBN         PIC  9(01).
      *                �𼨶�ʬ�������������ѡ�
                       09  SPA-COM-KOKUJISKBKBN1   PIC  9(01).
      *                �𼨶�ʬ�������������ѡ�
                       09  SPA-COM-KOKUJISKBKBN2   PIC  9(01).
      *    ��������
      *                �������ͷ׻�����
                       09  SPA-COM-KZMCOMPSIKIBETU     PIC 9(01).
      *                �������ͷ׻�����
                       09  SPA-COM-KZMKGN              PIC  9(08).
      *                �������ͷ׻����
                       09  SPA-COM-KZMJGN              PIC  9(08).
      *                ��������
                       09  SPA-COM-KZM                 PIC  9(08).
      *                ��������
                       09  SPA-COM-KZMTEN              PIC  9(07)V99.
      *                ��������
                       09  SPA-COM-KZMERR              PIC  9(01).
      *                ���������ʬ
                       09  SPA-COM-HOUKATUTEIGENKBN    PIC  9(03).
      *                ǯ��û�
                       09  SPA-COM-AGEKSNTBL           OCCURS 4.
      *                    ǯ��û�����
                           11  SPA-COM-AGEKSNKGN       PIC  X(02).
      *                    ǯ��û����
                           11  SPA-COM-AGEKSNJGN       PIC  X(02).
      *                    ǯ��û����ť�����
                           11  SPA-COM-AGEKSNSRYCD     PIC  X(09).
      *                ���������
                       09  SPA-COM-DAYCNT              PIC  9(01).
      *                �����оݶ�ʬ
                       09  SPA-COM-TEIGENKBN           PIC  9(01).
      *    �ȣ���������ʬ
      *                ��ȯ�����ʶ�ʬ
                       09  SPA-COM-KOUHATUKBN          PIC  9(01).
      *                �����Ŭ��������ʬ
                       09  SPA-COM-KIJUNTEIGENKBN      PIC  9(01).
      *                �����Ŭ�������оݻ��ߴ��
                       09  SPA-COM-KIJUNTEIGENCD       PIC  9(04).
      *                ��ѻ��ߴ����Ŭ��
                       09  SPA-COM-KIJUNCHK-FLG        PIC  9(01).
      *    �ȣ���������ʬ
      *                �������Ļ��û���ʬ
                       09  SPA-COM-LASERKBN            PIC  9(01).
      *                ��������νŻ��û���ʬ
                       09  SPA-COM-CHPMESUKSN          PIC  9(01).
      *                ����̾����Ϣ������
                       09  SPA-COM-SYOMEIKANREN        PIC  X(09).
      *    �ȣ���������ʬ
      *                ��������ѻٱ�û�
                       09  SPA-COM-GAZOOPESUP          PIC  9(01).
      *                ��켱�̶�ʬ
                       09  SPA-COM-MASUISKBKBN         PIC  9(01).
      *ver.4.6
      *                ��ˡ�ο��������߶�ʬ
                       09  SPA-COM-YCOMKBN             PIC  9(01).
      *ver.4.7
      *H24.4 ����ʬ��ver.4.7.0��������ѡ�
      *                ����̾���ܶ�ʬ�ʣ�������̾��
                       09  SPA-COM-IPN-KISAIKBN        PIC  9(01).
      *                ��������
                       09  SPA-COM-GEN-PRICE           PIC  9(09)V9(4).
      *                ������ॳ����
                       09  SPA-COM-YAKKAKJNCD          PIC  X(12).
      *H28.4 ����ʬ
      *                ��������ʬ
                       09  SPA-COM-SIPPUKBN        PIC  X(01).
      *                �������������̣�
                       09  SPA-COM-SIPRYO1         PIC 9(05).
                       09  SPA-COM-SIPRYO2         PIC 9(05).
      *                ��������������
                       09  SPA-COM-SIP-SURYO       PIC 9(05)V9(05).
      *                ����������ͭ
                       09  SPA-COM-SIPGEN          PIC X(01).
                       09  SPA-COM-SIPKBN          PIC X(01).
      *                ���۸��оݺ�
                       09  SPA-COM-SIPZAI          PIC X(01).
      *
      *                ϫ�ҡ������Բ�
                       09  SPA-COM-ROSAIKBN        PIC  X(01).
      *                ϫ�ҡ��ͻ�û�
                       09  SPA-COM-SISIKSN         PIC  X(01).
      *                �󿯽�Ū���ư�֥�˥���󥰲û�
                       09  SPA-COM-HISINKSN        PIC  X(01).
      *                �����¸Ʊ���ȿ��û�
                       09  SPA-COM-TOUKETUHOZONKSN PIC  X(01).
      *H30.4
      *                �������������ȿ�ɸ�ܲû�
                       09  SPA-COM-AKUSEIBYORIKSN  PIC  X(01).
      *R02.4
      *                �ϳ������û�
                       09  SPA-COM-SOGAIKOTEIKSN   PIC  X(01).
      *                Ķ�����ں��û�
                       09  SPA-COM-CHPSESSAKUKSN   PIC  X(01).
      *                �������ĺ���ʻ�ܶ�ʬ
                       09  SPA-COM-SASINJIHEISIKBN PIC  X(01).
      *                �ѹ�ǯ�������ʥޥ�������Ƚ���ѡ�
                       09  SPA-COM-CHGYMD          PIC  X(08).
      *
      *H29.1           �桼����������ʬ
                       09  SPA-COM-MASTER-CLASS    PIC  X(01).
      *
      *    �����ޥ����ղþ�����
      *                �η��ʬ
                       09  SPA-COM-SAIKETUKBN          PIC 9(01).
      *                ���󸡺�����ʬ
                       09  SPA-COM-ZENKNSKBN           PIC 9(01).
      *                ���ϥ����å���ʬ
                       09  SPA-COM-INPCHKKBN           PIC 9(01).
      *                ���̴���ñ��
                       09  SPA-COM-KANZANTANICD        PIC X(03).
      *                ���̴�����
                       09  SPA-COM-KANZANCHI           PIC 9(05)V9(05).
      *ver.4.7         ����ũ��
                       09  SPA-COM-SOURYOHENKBN        PIC  9(01).
      *    �����ޥ����������þ�����
      *                ��������
                       09  SPA-COM-NAIFUKUKBN          PIC 9(01).
      *                Ĺ����������
                       09  SPA-COM-CHOKINISSU          PIC 9(03).
                       09  SPA-COM-CHOKIERR            PIC 9(01).
      *                ��Ϳ��
                       09  SPA-COM-TOUYOERR            PIC 9(01).
                       09  SPA-COM-TOUYO-G             OCCURS   10.
                           11  SPA-COM-TOUYOKGNAGE     PIC X(02).
                           11  SPA-COM-TOUYOJGNAGE     PIC X(02).
                           11  SPA-COM-TOUYORYO1       PIC 9(05)V9(05).
                           11  SPA-COM-TOUYORYO2       PIC 9(05)V9(05).
      *                ���²��
                       09  SPA-COM-TUKIJGNKAISU        PIC  9(3).
      *                ������Ϳ��
                       09  SPA-COM-TUKIJGNTOUYORYO     PIC  9(5)V9(5).
      *    ���Զش�����
      *                �ش�����
                       09  SPA-COM-PTKINKI             PIC 9(01).
                       09  SPA-COM-PTKINKI-ARI         PIC 9(01).
                       09  SPA-COM-KINKSTYMD           PIC X(08).
                       09  SPA-COM-TOUYOYMD            PIC X(08).
                       09  SPA-COM-PTKINKICD           PIC 9(01).
      *v4.8  Ϣ³��������
                       09  SPA-COM-GMNINS              PIC X(01).
      *H28.2 ����̾ɽ���б�
      *                ����̾
      *??????????      09  SPA-COM-MEI-NAME            PIC X(78).
                       09  SPA-COM-MEI-NAME            PIC X(98).
      *                ����̾�ʽ������󵭺ܡ�
                       09  SPA-COM-IPN-NAME            PIC X(100).
                       09  SPA-COM-IPN-TANINAME        PIC X(02).
                       09  SPA-COM-MEI-KBN             PIC X(01).
      *H28.5           ����̾���ܶ�ʬ
                       09  SPA-COM-TNSP-KISAIKBN       PIC X(01).
      *                ����������ʬ
                       09  SPA-COM-PSY-CLASS           PIC X(01).
      *H31.4
      *                ��������Ĺ�������о�
                       09  SPA-COM-PSYCHO2             PIC X(01).
      *H28.9           �������޶�ʬ
                       09  SPA-COM-JISANYAKU           PIC X(01).
      *H30.9           �쥻�ץȵ��ܻ����ʬ
                       09  SPA-COM-RECEKISAI           PIC X(01).
                       09  SPA-COM-RECEAUTO            PIC X(01).
      *R01.5
      *                ����̾������
                       09  SPA-COM-GECODE              PIC X(12).
      *ver.4.7 ����ͽ��
                       09  SPA-COM-YOBI1               PIC X(10).
                       09  SPA-COM-YOBI2               PIC X(10).
                       09  SPA-COM-YOBI3               PIC S9(10)V9(5).
      * 