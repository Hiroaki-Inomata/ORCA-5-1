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
      *    �������           COPY (CPRCF010.INC)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     ���         10/12/15  ������������
      * 01.00.02     ���         18/05/18  ����ñ�Ȥν��פ���
      ********************************************************************
       01  L200TMP-REC.
      *�ˣţ�
           03  L200TMP-KEY.
               05  L200TMP-NYUGAIKBN-KEY    PIC  X(01).
               05  L200TMP-SRYKA-KEY        PIC  X(02).
               05  L200TMP-SORT-KEY         PIC  X(50).
               05  L200TMP-KEY2.
      *���ŵ��أɣ�
                   07  L200TMP-HOSPNUM          PIC  9(2).
      *����ǯ��
                   07  L200TMP-SRYYM            PIC  9(06).
      *������ʬ
                   07  L200TMP-NYUGAIKBN        PIC  X(01).
      *���ԣɣ�
                   07  L200TMP-PTID             PIC  9(10).
      *�쥻�ץȲʡʶ���λ������򥻥åȡ�
                   07  L200TMP-RECEKA           PIC  X(02).
      *������ʧ����
                   07  L200TMP-TEISYUTUSAKI     PIC  9(01).
      *�쥻�ץȼ���
                   07  L200TMP-RECESYUBETU      PIC  X(04).
      *�ݸ����ֹ�
                   07  L200TMP-HKNJANUM         PIC  X(08).
      *�����ʬ�������ݸ��ΤȤ��Τߡ�
                   07  L200TMP-HOJOKBN-KEY      PIC  X(01).
      *����ɣġʰ��ݡ�����ñ�ȤΤߡ�ϫ�ҡ������ա��ݸ��ɣġ�
                   07  L200TMP-KOHID-KEY        PIC  9(10).
      *Ŭ�ѳ���ǯ����
                   07  L200TMP-TEKSTYMD         PIC  X(08).
      *ʬ���ʬ
                   07  L200TMP-RECEIPT-KBN      PIC  X(10).
      *�����ֹ�
           03  L200TMP-PTNUM                PIC  X(20).
      *ǯ��
           03  L200TMP-AGE                  PIC  9(03).
      *���ճ��
           03  L200TMP-KYURATE              PIC  9(03).
      *�����Ƕ�ʬ
           03  L200TMP-ZAITAKU              PIC  X(01).
      *���ݸ����ݸ��ֹ�
           03  L200TMP-HKNNUM               PIC  X(03).
      *���ݸ����ܿͲ�²��ʬ
           03  L200TMP-HONKZKKBN           PIC  X(01).
      *Ϸ���ݸ����ݸ��ֹ�
           03  L200TMP-RJNHKNNUM            PIC  X(03).
      *��̾
           03  L200TMP-NAME                 PIC  X(50).
      *������ô���ֹ�
           03  L200TMP-FTNJANUM1            PIC  X(20).
           03  L200TMP-FTNJANUM2            PIC  X(20).
      *���������ֹ�
           03  L200TMP-JKYSNUM1             PIC  X(20).
           03  L200TMP-JKYSNUM2             PIC  X(20).
      *������
           03  L200TMP-TANJO                PIC  X(08).
      *������
           03  L200TMP-NISSU                PIC  9(02).
      *������
           03  L200TMP-TENSU                PIC  9(09).
      *��ô��
           03  L200TMP-ITBFTN               PIC  9(09).
      *��������
           03  L200TMP-SNISSU               PIC  9(02).
      *����������
           03  L200TMP-KINGK                PIC  9(09).
      *ɸ����ô��
           03  L200TMP-FTNGK                PIC  9(09).