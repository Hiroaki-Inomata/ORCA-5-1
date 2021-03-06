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
      *  ��ʥơ��֥�       (CPSYUKA.INC)
      *****************************************************************
      *  ��������
      * Maj/Min/Rev  ������       ����      ����
      *  03.02.01    NACL-ƣ��    06/08/11  �����ֹ���ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     kohid �ɲ�
      *  04.06.02    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *                                     receipt_kbn �ɲ� 
      *****************************************************************
      *
      *01  SYUKA-REC.
           03  SYUKA-KEY.
      *���ŵ��أɣ�
               05  SYUKA-HOSPNUM        PIC 9(2).
      *������ʬ
               05  SYUKA-NYUGAIKBN     PIC X(01).
      *���ԣɣ�
               05  SYUKA-PTID          PIC 9(10).
      *��׶�ʬ
               05  SYUKA-TEN-KBN       PIC X(01).
      *����ǯ��
               05  SYUKA-SRYYM         PIC X(06).
      *������ʧ����
               05  SYUKA-TEISYUTUSAKI  PIC X(01).
      *�ݸ����ֹ�
               05  SYUKA-HKNJANUM      PIC X(08).
      *�����ʬ
               05  SYUKA-HOJOKBN       PIC X(01).
      *����ɣ�
               05  SYUKA-KOHID         PIC 9(10).
      *ʬ���ʬ
               05  SYUKA-RECEIPT-KBN   PIC X(10).
      *�쥻�ץȼ���
               05  SYUKA-RECESYUBETU   PIC X(04).
      *���Ų�
               05  SYUKA-SRYKA         PIC X(02).
      *�ݸ��ֹ�ʼ��ݸ���
           03  SYUKA-HKNNUM            PIC X(03).
      *���Ųʡ����ϡ�
           03  SYUKA-SRYKA-INPUT       PIC X(02).
      *���Ųʡʥ����ƥ��
           03  SYUKA-SRYKA-AUTO        PIC X(02).
      *�ݸ��ȹ礻�ֹ�
           03  SYUKA-HKNCOMBI-AREA.
               05  SYUKA-HKNCOMBI      PIC 9(04)   OCCURS  10.
      *�����������
           03  SYUKA-TENSU             PIC S9(09).
      *���ǲ�����(������)
           03  SYUKA-KAISU             PIC 9(03).
      *�̻��������(����)
           03  SYUKA-NISSU             PIC 9(03).
      *��λ��
           03  SYUKA-EDDAY             PIC 9(02).
      *̤����ե饰
           03  SYUKA-MISEIKYU-FLG      PIC X(02).
      *����ե饰
           03  SYUKA-DLT-FLG           PIC X(01).
      *ü���ɣ�
           03  SYUKA-TERMID            PIC X(16).
      *���ڡ��졼���ɣ�
           03  SYUKA-OPID              PIC X(16).
      *����ǯ����
           03  SYUKA-CREYMD            PIC X(08).
      *����ǯ����
           03  SYUKA-UPYMD             PIC X(08).
      *��������
           03  SYUKA-UPHMS             PIC X(06).
      *�����ѹ���
           03  SYUKA-PTNUM             PIC X(20).
