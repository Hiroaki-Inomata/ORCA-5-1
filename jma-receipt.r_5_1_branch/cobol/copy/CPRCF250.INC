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
      *    �����쥻�ץȡ������ˡ����ౡ�������  COPY (CPRCF250.INC) 
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.07.01    NACL-ƣ��    15/01/24  Ʊ���������������ౡ����ε�Ͽ
      *                                     ����
      *                                    �����������ౡ����Ʊ���Ȥ���
      ************************************************************
      *01  RECE250-REC.
           03  RECE250-KEY.
               05  RECE250-KEY1.
      *���ŵ����ֹ�
                   07  RECE250-HOSPNUM     PIC 9(2).
      *����ǯ��
                   07  RECE250-SRYYM       PIC 9(06).
      *������ʬ
                   07  RECE250-NYUGAIKBN   PIC X(01).
      *���ԣɣ�
                   07  RECE250-PTID        PIC 9(10).
      *�쥻�ץȿ��Ų�
                   07  RECE250-RECEKA      PIC X(02).
      *����ɣ�
                   07  RECE250-HKNID       PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE250-TEKSTYMD    PIC X(08).
      *����ǯ����
               05  RECE250-NYUINYMD        PIC X(08).
      *����ǯ����Ϣ��
               05  RECE250-NYUINYMD-SEQNO  PIC 9(01).
      *�ౡǯ����
           03  RECE250-TAIINYMD            PIC X(08).
      *��������������ʬ
           03  RECE250-NYUINCHUKBN         PIC X(01).
