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
      *          ����������� COPY (CPRMF100.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      01  CSV-REC.
      *�쥳���ɶ�ʬ
           03  CSV-RECKBN         PIC  9(01).
           03  FILLER             PIC  X(02)  VALUE  ",""".
      *���ŵ����ֹ�
           03  CSV-HOSPNUM        PIC  X(10).
           03  FILLER             PIC  X(02)  VALUE  """,".
      *���Ųʥ�����
           03  CSV-SRYKA          PIC  9(02).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *����ǯ��
           03  CSV-SRYYM          PIC  9(05).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *��Į¼�ֹ�
           03  CSV-CITYNUM        PIC  9(02).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *�����ֹ�
           03  CSV-JIGYONUM       PIC  9(01).
           03  FILLER             PIC  X(02)  VALUE  ",""".
      *������ֹ�
           03  CSV-JKYSNUM        PIC  X(09).
           03  FILLER             PIC  X(03)  VALUE  """,""".
      *��̾
           03  CSV-NAME           PIC  X(30).
           03  FILLER             PIC  X(02)  VALUE  """,".
      *��ǯ����
           03  CSV-BIRTHDAY       PIC  9(07).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *�ݸ���ʬ
           03  CSV-HKNKBN         PIC  9(01).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *�ݸ����ֹ�
           03  CSV-HKNJANUM       PIC  9(08).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *������ʬ
           03  CSV-NYUGAIKBN      PIC  9(01).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *���
           03  CSV-RATE           PIC  9(01).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *������
           03  CSV-JNISSU         PIC  9(02).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *�������
           03  CSV-TENSU          PIC  9(10).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *������ô��ʧ��
           03  CSV-ITBFTN         PIC  9(10).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *�¿���ǯ��
           03  CSV-JSRYYM         PIC  9(05).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *�����ֹ棱
           03  CSV-KOHNUM1        PIC  9(02).
           03  FILLER             PIC  X(01)  VALUE  ",".
      *�����ֹ棲
           03  CSV-KOHNUM2        PIC  9(02).
           03  FILLER             PIC  X(01)  VALUE  X"0D".
