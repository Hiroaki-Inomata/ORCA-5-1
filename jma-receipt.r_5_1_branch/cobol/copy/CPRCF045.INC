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
      *    �쥻�ץ����ٽ�     COPY (CPRCF045.INC)                *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    03.01.22  �ǡ�����ʬ���ɲ�
      *  01.00.02    NACL-����    03.03.03  ���ֹ�彤��
      *  01.00.03    NACL-����    03.09.29  ����оݸ���������ɲ�
      *                                     �������»�Ƚ�Ǿ�����ɲ�
      *                                     �Ƽ��ֹ���������ɲ�
      *  01.00.04    NACL-����    04.06.07  �����֤����Ĺ��ܤ��ѹ�
      *
      *  02.06.01    NACL-ƣ��    05/08/22  ���θ��������Ȥ��ɲ�
      *
      *  02.09.01    NACL-ƣ��    06/04/13  �������ͷ׻�����
      *                                     �������»�Ƚ�ǥ��롼�פ��ɲ�
      *  03.05.01    NACL-����    07/05/28  ���롼�׿����б�
      *  04.01.01    NACL-����    07/11/26  �����ྦ��̾�����б�
      *
      *  04.05.01    NACL-ƣ��    09/07/14  �������ʲ������б�
      *
      *  04.07.01    NACL-����    13/01/21  ϫ�ҥ쥻���б�
      *  04.07.02    NACL-����    14/09/11  �����ޥ����ο��Ŷ�ʬ�ɲ�
      *
      *  04.08.01    NACL-����    16/01/12  �����ѥ쥻�ץȱ�������
      *                                     ����̾�����б�
      *  04.08.02    NACL-����    16/04/20  ʿ������ǯ��������б�
      *
      *  05.00.01    NACL-����    20/03/30  ���£�ǯ��������б�
      ************************************************************
      *01  RECE45-REC.
           03  RECE45-KEY.
               05  RECE45-KEY1.
      *���ŵ��أɣ�
                   07  RECE45-HOSPNUM     PIC  9(02).
      *����ǯ��
                   07  RECE45-SRYYM       PIC  9(06).
      *������ʬ
                   07  RECE45-NYUGAIKBN   PIC  X(01).
      *���ԣɣ�
                   07  RECE45-PTID        PIC  9(10).
      *�쥻�ץȲ�
                   07  RECE45-RECEKA      PIC  X(02).
      *�ݸ��ɣ�        
                   07  RECE45-HKNID       PIC  9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE45-TEKSTYMD    PIC  X(08). 
      *���ֹ�
               05  RECE45-ZAINUM          PIC  9(08).
      *Ϣ�ֹ�
               05  RECE45-RENNUM          PIC  9(02).
      *�����ֹ�
           03  RECE45-PTNUM               PIC  X(20).
      *���Ų�
           03  RECE45-SRYKA               PIC  X(02).
      *���Ŷ�ʬ
           03  RECE45-SRYKBN              PIC  X(02).
      *���ż��̶�ʬ
           03  RECE45-SRYSYUKBN           PIC  X(03).
      *������
           03  RECE45-JIHIMONEY           PIC  9(07).
      *
           03  RECE45-SRYACT-TBL          OCCURS  5.
      *���Ź԰پ���
             05  RECE45-SRYACTINF.
      *���ť�����
                 07  RECE45-SRYCD         PIC  X(09).
      *���ż��̶�ʬ
                 07  RECE45-SRYSYUKBN-X   PIC  X(03).
      *����
                 07  RECE45-SRYSURYO      PIC  9(05)V9(05).
      *���
                 07  RECE45-SRYKAISU      PIC  9(03).
      *ñ�̥�����
                 07  RECE45-TANICD        PIC  9(03).
      *ñ��̾��ͭ�����
                 07  RECE45-TANIMOJISU    PIC  9(01).
      *ñ��̾��
                 07  RECE45-TANINAME      PIC  X(24).
      *�������̶�ʬ
                 07  RECE45-TENSIKIBETU   PIC  9(01).
      *����
                 07  RECE45-TEN           PIC  9(09)V9(02).
      *��������ե饰
                 07  RECE45-MEISKYFLG     PIC  X(01).
      *��ư�����ʬ
                 07  RECE45-AUTOKBN       PIC  X(01).
      *̾�������ֹ�
                 07  RECE45-INPUTNUM      PIC  9(03).
      *�����󽸷��輱��
                 07  RECE45-TENTTLKBN     PIC  9(03).
      *�ǡ�����ʬ
                 07  RECE45-DATAKBN       PIC  9(01).
      *����оݸ���
                 07  RECE45-HOUKNSKBN     PIC  9(02).
      *�������»�Ƚ��
                 07  RECE45-KNSJISKBN     PIC  9(01).
      *�������»�Ƚ�ǥ��롼��
                 07  RECE45-KNSJISGRPKBN  PIC  9(02).
      *�Ƽ��ֹ���
                 07  RECE45-CDKBNINF.
                     09  RECE45-CDKBN-KBN        PIC  X(01).
                     09  RECE45-CDKBN-SYO        PIC  9(01).
                     09  RECE45-CDKBN-BU         PIC  9(02).
                     09  RECE45-CDKBN-KBNNUM     PIC  9(03).
                     09  RECE45-CDKBN-KBNNUM-EDA PIC  9(02).
                     09  RECE45-CDKBN-KOUBAN     PIC  9(02).
                     09  RECE45-KOUHYOJYUNNUM    PIC  9(09).
      *�������̶�ʬ��
                 07  RECE45-KOKUJISKBKBN1 PIC  9(01).
      *�������̶�ʬ��
                 07  RECE45-KOKUJISKBKBN2 PIC  9(01).
      *��§�û��оݳ���ʬ
                 07  RECE45-TUSOKUGAIKBN  PIC  9(01).
      *���ֳ��û���ʬ
                 07  RECE45-TIMEKSNKBN    PIC  9(01).
      *�������ͷ׻�����
                 07  RECE45-KZMCOMPSIKIBETU
                                          PIC  9(01).
      *���û�������
                 07  RECE45-CHUKSNINF.
      *������
                     09  RECE45-CHUKSNCD         PIC  X(04).
      *����
                     09  RECE45-CHUKSNTUBAN      PIC  X(01).
      *���θ���������
                 07  RECE45-KENTAICOMMENT        PIC  9(01).
      *���ϥ�����
                 07  RECE45-INPUTCOMENT          PIC  X(140).
      *̾�ξ���
             05  RECE45-NAMEINF.
      *����ͭ�����
                 07  RECE45-YUKOKETA      PIC  9(03).
      *����̾��
                 07  RECE45-NAME          PIC  X(300).
      *���̥ե饰����
             05  RECE45-SIKFLGINF.
      *���̥ե饰
                 07  RECE45-SIKFLG        PIC  X(01).
      *��������̣�
             05  RECE45-TOKUKIZAISBT1     PIC  9(01).
      *��������ʬ
             05  RECE45-SANSOKBN          PIC  9(01).
      *�쥻�ץ��Խ�����
             05  RECE45-SSTKIJUNCDTBL     OCCURS 10.
                 07  RECE45-SSTKIJUNCD    PIC  9(03).
      *����¾����
             05  RECE45-SONOTAINF.
      *�쥻�ץ��Ż��оݳ��ե饰
                 07  RECE45-RECEDENFLG       PIC  X(01).
      *�쥻�ץ��оݳ��ե饰
                 07  RECE45-RECEPTFLG        PIC  X(01).
      *�쥻���������ྦ��̾
                 07  RECE45-RECEDENNAME      PIC  X(300).
      *�쥻�������ྦ��̾�Խ��ե饰
                 07  RECE45-RECEHENFLG       PIC  X(01).
      *�ط������ȴ�Ϣ����ե饰
                 07  RECE45-SETCOMATFLG      PIC  9(03).
      *�����ޥ����ο��Ŷ�ʬ
                 07  RECE45-TNSSRYKBN        PIC  X(02).
      *
      *���ž���Ϣ���Ѿ���
             05  RECE45-IRYORENKEIINF.
      *����̾���ܱ��������ե饰
                 07  RECE45-INGAIFLG         PIC  X(01).
      *����̾���ܱ��������ե饰�ʥ쥻�ץ��ѡ�
                 07  RECE45-INGAIFLG-RECE    PIC  X(01).
      *����̾���ܱ��������ե饰�ʼ������������ѡ�
                 07  RECE45-INGAIFLG-SIPPU   PIC  X(01).
      *����̾�����ɡ�������ॳ���ɡ�
                 07  RECE45-YAKKAKJNCD       PIC  X(12).
      *����̾�����û���ʬ
                 07  RECE45-GENERIC-KBN      PIC  X(01).
      *����̾��
                 07  RECE45-GENERICNAME      PIC  X(200).
      *����ͭ�����
                 07  RECE45-GENERICYUKOKETA  PIC  9(03).
      *
      *����������
             05  RECE45-SIPPUINF.
                 07  RECE45-SIPPUFLG         PIC  X(01).
      *