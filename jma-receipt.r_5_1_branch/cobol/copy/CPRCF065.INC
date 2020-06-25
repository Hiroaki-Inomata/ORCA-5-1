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
      *    �쥻�ץ����ٽ�     COPY (CPRCF065.INC)                *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    03.01.17  �ǡ�����ʬ���ɲ�
      *  01.00.02    NACL-����    03.09.29  ����оݸ���������ɲ�
      *                                     �������»�Ƚ�Ǿ�����ɲ�
      *                                     �Ƽ��ֹ���������ɲ�
      *  01.00.03    NACL-����    04.06.07  �����֤����Ĺ��ܤ��ѹ�
      *
      *  01.00.04    NACL-ƣ��    04/07/27  ���ż��̶�ʬ
      *                                     �������̶�ʬ��
      *                                     �������ͷ׻�����
      *                                     ���û������ɤ��ɲ�
      *  01.00.05    NACL-����    05/01/17  ����¾������ɲ�
      *
      *  02.06.01    NACL-ƣ��    05/08/22  ���θ��������Ȥ��ɲ�
      *
      *  02.09.01    NACL-ƣ��    06/04/13  �������»�Ƚ�ǥ��롼�פ��ɲ�
      *  03.04.01    NACL-����    07/02/19  �쥻�ץ��оݳ��ե饰���ɲ�
      *  03.04.02    NACL-ƣ��    07/03/27  ��������̣�����������ʬ���ɲ�
      *  03.05.01    NACL-��Ʀ��  07/05/30  ���롼�׿����б�
      *  04.01.01    NACL-����    07/11/26  �����ྦ��̾�����б�
      *
      *  04.04.01    NACL-ƣ��    09/04/21  �������̶�ʬ������§�û���
      *                                     �ݳ���ʬ���ɲ�
      *  04.04.02    NACL-ƣ��    09/12/07  ���ֳ��û���ʬ���ɲ�
      *
      *  04.05.01    NACL-ƣ��    09/07/14  �������ʲ������б�
      *  04.05.02    NACL-����    09/11/30  �쥻�ťե����ޥåȥ����ȳ�ĥ
      *                                     �б��ʥե������Խ���
      *
      *  04.06.01    NACL-ƣ��    10/06/23  �쥻��������μ��ե��顼ʬ��
      *                                     ���������쥻�б�
      *  04.06.02    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *
      *  04.07.01    NACL-ƣ��    12/12/17  ʣ����쥻�ץ�ʬ���б�
      *
      *  04.08.01    NACL-����    16/01/12  �����ѥ쥻�ץȱ�������
      *                                     ����̾�����б�
      *
      *  05.00.01    NACL-����    20/03/30  ���£�ǯ��������б�
      ************************************************************
      *01  RECE65-REC.
           03  RECE65-KEY.
               05  RECE65-KEY1.
      *���ŵ��أɣ�
                   07  RECE65-HOSPNUM      PIC 9(2).
      *����ǯ��
                   07  RECE65-SRYYM        PIC 9(06).
      *������ʬ
                   07  RECE65-NYUGAIKBN    PIC X(01).
      *���ԣɣ�
                   07  RECE65-PTID         PIC 9(10).
      *�쥻�ץȲ�
                   07  RECE65-RECEKA       PIC X(02).
      *������ʧ����
                   07  RECE65-TEISYUTUSAKI
                                           PIC 9(01).
      *�쥻�ץȼ���
                   07  RECE65-RECESYUBETU  PIC 9(04).
      *�ݸ����ֹ�        
                   07  RECE65-HKNJANUM     PIC X(08).
      *�����ʬ
                   07  RECE65-HOJOKBN      PIC X(01).
      *����ɣ�
                   07  RECE65-KOHID        PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE65-TEKSTYMD     PIC X(08). 
      *ʬ���ʬ
                   07  RECE65-RECEIPT-KBN  PIC X(10).
      *���ֹ�
               05  RECE65-ZAINUM          PIC  9(08).
      *��Ϣ��
               05  RECE65-ZAIREN          PIC  9(02).
      *Ϣ�ֹ�
               05  RECE65-RENNUM          PIC  9(02).
      *����ʬΥ��ʬ
           03  RECE65-KOHBUNRI            PIC  9(01).
      *�����ֹ�
           03  RECE65-PTNUM               PIC  X(20).
      *���Ų�
           03  RECE65-SRYKA               PIC  X(02).
      *���Ŷ�ʬ
           03  RECE65-SRYKBN              PIC  X(02).
      *���ż��̶�ʬ
           03  RECE65-SRYSYUKBN           PIC  X(03).
      *������
           03  RECE65-JIHIMONEY           PIC  9(07).
      *
           03  RECE65-SRYACT-TBL          OCCURS  5.
      *���Ź԰پ���
               05  RECE65-SRYACTINF.
      *���ť�����
                   07  RECE65-SRYCD           PIC  X(09).
      *���ż��̶�ʬ
                   07  RECE65-SRYSYUKBN-X     PIC  X(03).
      *����
                   07  RECE65-SRYSURYO        PIC  9(05)V9(05).
      *���
                   07  RECE65-SRYKAISU        PIC  9(03).
      *ñ�̥�����
                   07  RECE65-TANICD          PIC  9(03).
      *ñ��̾��ͭ�����
                   07  RECE65-TANIMOJISU      PIC  9(01).
      *ñ��̾��
                   07  RECE65-TANINAME        PIC  X(24).
      *�������̶�ʬ
                   07  RECE65-TENSIKIBETU     PIC  9(01).
      *����
                   07  RECE65-TEN             PIC  9(09)V9(02).
      *��������ե饰
                   07  RECE65-MEISKYFLG       PIC  X(01).
      *��ư�����ʬ
                   07  RECE65-AUTOKBN         PIC  X(01).
      *̾�������ֹ�
                   07  RECE65-INPUTNUM        PIC  9(03).
      *�����󽸷��輱��
                   07  RECE65-TENTTLKBN       PIC  9(03).
      *�ǡ�����ʬ
                   07  RECE65-DATAKBN         PIC  9(01).
      *����оݸ���
                   07  RECE65-HOUKNSKBN       PIC  9(02).
      *�������»�Ƚ��
                   07  RECE65-KNSJISKBN       PIC  9(01).
      *�������»�Ƚ�ǥ��롼��
                   07  RECE65-KNSJISGRPKBN    PIC  9(02).
      *�Ƽ��ֹ���
                   07  RECE65-CDKBNINF.
                       09  RECE65-CDKBN-KBN        PIC  X(01).
                       09  RECE65-CDKBN-SYO        PIC  9(01).
                       09  RECE65-CDKBN-BU         PIC  9(02).
                       09  RECE65-CDKBN-KBNNUM     PIC  9(03).
                       09  RECE65-CDKBN-KBNNUM-EDA PIC  9(02).
                       09  RECE65-CDKBN-KOUBAN     PIC  9(02).
                       09  RECE65-KOUHYOJYUNNUM    PIC  9(09).
      *�������̶�ʬ��
                   07  RECE65-KOKUJISKBKBN1   PIC  9(01).
      *�������̶�ʬ��
                   07  RECE65-KOKUJISKBKBN2   PIC  9(01).
      *��§�û��оݳ���ʬ
                   07  RECE65-TUSOKUGAIKBN    PIC  9(01).
      *���ֳ��û���ʬ
                   07  RECE65-TIMEKSNKBN      PIC  9(01).
      *�������ͷ׻�����
                   07  RECE65-KZMCOMPSIKIBETU PIC  9(01).
      *���û�������
                   07  RECE65-CHUKSNINF.
      *������
                       09  RECE65-CHUKSNCD     PIC  X(04).
      *����
                       09  RECE65-CHUKSNTUBAN  PIC  X(01).
      *���θ���������
                   07  RECE65-KENTAICOMMENT    PIC  9(01).
      *���ϥ�����
                   07  RECE65-INPUTCOMENT      PIC  X(140).
      *̾�ξ���
               05  RECE65-NAMEINF.
      *����ͭ�����
                   07  RECE65-YUKOKETA         PIC  9(03).
      *����̾��
                   07  RECE65-NAME             PIC  X(300).
      *��������̣�
               05  RECE65-TOKUKIZAISBT1        PIC  9(01).
      *��������ʬ  
               05  RECE65-SANSOKBN             PIC  9(01).
      *�쥻�ץ��Խ�����
               05  RECE65-SSTKIJUNCDTBL             OCCURS 10.
                   07  RECE65-SSTKIJUNCD       PIC  9(03).
      *����¾����
               05  RECE65-SONOTAINF.
      *�쥻�ץ��Ż��оݳ��ե饰
                   07  RECE65-RECEDENFLG       PIC  X(01).
      *�쥻�ץ��оݳ��ե饰
                   07  RECE65-RECEPTFLG        PIC  X(01).
      *�쥻���������ྦ��̾
                   07  RECE65-RECEDENNAME      PIC  X(300).
      *�쥻�������ྦ��̾�Խ��ե饰
                   07  RECE65-RECEHENFLG       PIC  X(01).
      *�ط������ȴ�Ϣ����ե饰
                   07  RECE65-SETCOMATFLG      PIC  9(03).
      *
      *���ž���Ϣ���Ѿ���
               05  RECE65-IRYORENKEIINF.
      *����̾���ܱ��������ե饰
                   07  RECE65-INGAIFLG         PIC  X(01).
      *����̾���ܱ��������ե饰�ʥ쥻�ץ��ѡ�
                   07  RECE65-INGAIFLG-RECE    PIC  X(01).
      *����̾�����ɡ�������ॳ���ɡ�
                   07  RECE65-YAKKAKJNCD       PIC  X(12).
      *����̾�����û���ʬ
                   07  RECE65-GENERIC-KBN      PIC  X(01).
      *����̾��
                   07  RECE65-GENERICNAME      PIC  X(200).
      *����ͭ�����
                   07  RECE65-GENERICYUKOKETA  PIC  9(03).
      *