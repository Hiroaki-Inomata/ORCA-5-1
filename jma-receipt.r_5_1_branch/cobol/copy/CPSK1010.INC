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
      *         �����ƥ�����ޥ���     (CPSK1010.INC)   *
      *         �ʣ���������                            *
      *         ��������                                *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 00-00-01     MCC-����     02/05/07  ͽ���ν���
      *                                     �����饤���󥹤��ɲ�
      * 01.00.02     NACL-¿��Ǽ  05/12/01  �����ȯ�ԥե饰���ɲ�
      * 01.00.03     NACL-¿��Ǽ  06/04/10  ����������ȯ���ɲ�
      * 03.04.00     NACL-¿��Ǽ  06/11/13  ��������̾��ɽ���ե饰�ɲ�
      * 03.05.01     NACL-ƣ��    07/09/14  ���饤�������¸���ݥե饰�ɲ�
      * 04.04.00     NACL-��Ʀ��  09/01/27  �ܱ�ʬ���б���¾����������ɣ��ɲá�
      * 04.05.00     NACL-����    09/11/25  �桼�����������󸢸��ɲ�
      * 04.08.00     NACL-¿��Ǽ  15/10/27  ɽ����ʬ�ɲ�
      ************************************************************
       01  SYS-1010-REC.
           03  SYS-1010-KEY.
               05  SYS-1010-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1010-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1010-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1010-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1010-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-1010-TBL.
      *���ڥ졼���ɣ�
               05  SYS-1010-USERID      PIC  X(16).
      *�եꥬ��
               05  SYS-1010-KANANAME    PIC  X(80).
      *��̾
               05  SYS-1010-NAME        PIC  X(80).
      *������Ͽ�ֹ�
               05  SYS-1010-ISEKINUM    PIC  X(08).
      *�ݸ�����Ͽ�ֹ�ʼ��ݡ�
               05  SYS-1010-HKNDRNUMS   PIC  X(08).
      *�ݸ�����Ͽ�ֹ�ʹ��ݡ�
               05  SYS-1010-HKNDRNUMK   PIC  X(12).
      *����ʥ�����
               05  SYS-1010-SEMONKACD  OCCURS  5.
                   07  SYS-1010-SEMONKACD-X  PIC  X(02).
      *��̳��������
               05  SYS-1010-GSRAUTH     PIC  X(70).
      *�����Ը��¥ե饰
               05  SYS-1010-KANRIAUTHFLG  PIC  X(01).
      *�����饤����
               05  SYS-1010-MAYAKULICENSE    PIC  X(40).
      *���Ź԰ٽ��ɽ���ե饰
               05  SYS-1010-ORCA21FLG     PIC  X(01).
      *
      *�����ȯ�ԥե饰
               05  SYS-1010-SKYPRTFLG      PIC  X(01).
      *������������ȯ�ԥե饰
               05  SYS-1010-SYOHOPRTFLG    PIC  X(01).
      *���޾���ȯ�ԥե饰
               05  SYS-1010-YAKUJYOPRTFLG  PIC  X(01).
      *���������ٽ�ȯ�Զ�ʬ�ե饰
               05  SYS-1010-MEIPRTFLG      PIC  X(01).
      *��������̾��ɽ���ե饰
               05  SYS-1010-FORMALFLG      PIC  X(01).
      *���饤�������¸���ݥե饰
               05  SYS-1010-CLIENT-SAVE    PIC  X(01).
      *���롼�װ��ŵ��إ����������
               05  SYS-1010-GRPHOSPINF-G.
                   07  SYS-1010-GRPHOSPINF     OCCURS  9.
                       09  SYS-1010-GRPHOSPNUMX   PIC  X(02).
                       09  SYS-1010-GRPHOSPNUM    REDEFINES
                           SYS-1010-GRPHOSPNUMX   PIC  9(02).
                       09  SYS-1010-GRPHOSPUSERID PIC  X(16).
      *�桼�����������󸢸¥ե饰
               05  SYS-1010-USER-KANRI     PIC  X(01).
      *���饤����Ȱ������ݥե饰
               05  SYS-1010-CLIENT-PRT     PIC  X(01).
      *����������ɽ���ե饰
               05  SYS-1010-DIALOGFLG      PIC  X(01).
      *ɽ����ʬ
               05  SYS-1010-HYOJIFLG       PIC  X(01).
      *ͽ��
      **** 03                           PIC  X(175).
      *****03                           PIC  X(174).
      *****03                           PIC  X(168).
      *****03                           PIC  X(06).
      *****03                           PIC  X(05).
      *****03  SYS-1010-YOBI            PIC  X(03).
           03  SYS-1010-YOBI            PIC  X(02).
      *ü���ɣ�
           03  SYS-1010-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1010-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1010-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1010-UPYMD           PIC  X(08).
      *��������
           03  SYS-1010-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1010-UPD-KEY.
               05  SYS-1010-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1010-UPD-KANRICD     PIC X(04).
               05  SYS-1010-UPD-KBNCD       PIC X(08).
               05  SYS-1010-UPD-STYUKYMD    PIC X(08).
               05  SYS-1010-UPD-EDYUKYMD    PIC X(08).