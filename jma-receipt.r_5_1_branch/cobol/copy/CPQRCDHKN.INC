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
      *  QR������(���ݸ��Ծ�)��Ǽ��               (CPQRCDHKN.INC) 
      *****************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  QRCDHKN-REC.
      *
      *����
           03  QRCDHKN-KEY.
      *���ŵ��أɣ�
               05  QRCDHKN-HOSPNUM         PIC 9(2).
      *����ǯ����
               05  QRCDHKN-REGISTYMD       PIC X(08).
      *���գɣ�
               05  QRCDHKN-REGISTID       PIC 9(05).
      *���ջ���
           03  QRCDHKN-REGISTTIME          PIC X(06).
      *���ٶ�ʬ(ɬ��)
           03  QRCDHKN-SEIDOKBN            PIC X(03).
      *�ݸ����ֹ�(ɬ��)
           03  QRCDHKN-HKNJANUM            PIC X(08).
      *�����ֹ�(ɬ��)
           03  QRCDHKN-KIGONUM             PIC X(40).
      *�ܿͲ�²��ʬ(ɬ��)
           03  QRCDHKN-HONKZKKBN           PIC X(01).
      *���ʻ�̾(Ǥ��)�����ѡ�
           03  QRCDHKN-KANANAME            PIC X(100).
      *����(ɬ��)
           03  QRCDHKN-SEX                 PIC X(01).
      *��ǯ����(ɬ��)
           03  QRCDHKN-BIRTHDAY            PIC X(07).
      *-------------> �����ޤǸ���Ĺ
      *�ݸ����ȼ��δ����ֹ�(Ǥ��)
           03  QRCDHKN-KANRINUM            PIC X(100).
      *������̾(Ǥ��)
           03  QRCDHKN-NAME                PIC X(100).
      *���ճ��(Ǥ��)
           03  QRCDHKN-KYURATE             PIC X(02).
      *��ȯ�ԥ�����(Ǥ��)
           03  QRCDHKN-SAIHAKKU            PIC X(50).
      *-------------> �����ޤ����Ͼ���
      *���ԣɣ�
           03  QRCDHKN-PTID                PIC 9(10).
      *����
           03  QRCDHKN-KIGO                PIC  X(80).
      *�ֹ�
           03  QRCDHKN-NUM                 PIC  X(80).
      *��ǯ����(����)
           03  QRCDHKN-BIRTHDAYS           PIC X(08).
      *���顼������
           03  QRCDHKN-ERRCD               PIC X(02).
      *�����ѥե饰
           03  QRCDHKN-MOD-FLG             PIC 9(01).
      *���պѥե饰
           03  QRCDHKN-MOD-FLG2            PIC 9(01).
      *�����ʬ
           03  QRCDHKN-DELKBN              PIC X(01).
      *ü���ɣ�
           03  QRCDHKN-TERMID              PIC X(64).
      *���ڡ��졼���ɣ�
           03  QRCDHKN-OPID                PIC X(16).
      *����ǯ����
           03  QRCDHKN-CREYMD              PIC X(08).
      *����ǯ����
           03  QRCDHKN-UPYMD               PIC X(08).
      *��������
           03  QRCDHKN-UPHMS               PIC X(06).