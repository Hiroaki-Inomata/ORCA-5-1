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
       01  SC17.
      *���ŵ���CD
           03  SC17-HOSPCD                 PIC  X(14).
      *���ŵ��ؽ����
           03  SC17-ADRS                   PIC  X(100).
      *���ŵ���̾��
           03  SC17-HOSPNAME               PIC  X(100).
      *�����ֹ�
           03  SC17-TEL                    PIC  X(15).
      *����ǯ��
           03  SC17-PRTYM                  PIC  X(12).
      *��Į¼̾
           03  SC17-CITYNAME               PIC  X(16).
      *���ټ���
           03  SC17-KOHKBN                 PIC  X(02) OCCURS 6.
      *��Į¼�ֹ�
           03  SC17-CITYNUM.
               05  SC17-CITYNUM-OCC        PIC  X(01) OCCURS 4.
      *������ֹ�
           03  SC17-JKYSNUM.
               05  SC17-JKYSNUM-OCC        PIC  X(01) OCCURS 7.
      *��̾
           03  SC17-NAME                   PIC  X(24).
      *���ʻ�̾
           03  SC17-KANANAME               PIC  X(30).
      *����
           03  SC17-SEX                    PIC  X(02) OCCURS 2.
      *����
           03  SC17-GENGO                  PIC  X(02) OCCURS 4.
      *��ǯ����
           03  SC17-TANJOYMD               PIC  X(18).
      *���ճ��
           03  SC17-KYURATE                PIC  X(02) OCCURS 4.
      *�ݸ����ֹ�
           03  SC17-HKNJANUM.
               05  SC17-HKNJANUM-OCC       PIC  X(01) OCCURS 8.
      *Ϸ�ͼ�����ֹ�
           03  SC17-RJNJKYSNUM.
               05  SC17-RJNJKYSNUM-OCC     PIC  X(01) OCCURS 7.
      *����
           03  SC17-TENSU                  PIC  X(07) OCCURS 2.
      *�ݸ���ô��
           03  SC17-HKNGAKU                PIC  X(09) OCCURS 2.
      *����
           03  SC17-NISSU                  PIC  X(02) OCCURS 2.
      *������ô��
           03  SC17-ITBFTN                 PIC  X(07) OCCURS 2.
      *��������
           03  SC17-SNISSU                 PIC  X(02) OCCURS 4.
      *ɸ����ô��
           03  SC17-SFTNGK                 PIC  X(06) OCCURS 5.
      *����
           03  SC17-BIKO                   PIC  X(02) OCCURS 3.
      *��������
           03  SC17-TAHOTEN                PIC  X(07).
      *���񴵼���ô��
           03  SC17-TAHOFTN                PIC  X(07).
      *¾ˡ
           03  SC17-TAHO                   PIC  X(36).
      *���ٳ�
           03  SC17-GENDO                  PIC  X(02) OCCURS 5.
