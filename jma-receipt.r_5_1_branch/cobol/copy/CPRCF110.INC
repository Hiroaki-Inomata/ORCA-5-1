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
      *    �����쥻�ץȡ���̾����         COPY (CPRCF110.INC) 
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  RECE110-REC.
           03  RECE110-KEY.
               05  RECE110-KEY1.
      *���ŵ����ֹ�
                   07  RECE110-HOSPNUM     PIC 9(2).
      *����ǯ��
                   07  RECE110-SRYYM       PIC 9(06).
      *������ʬ
                   07  RECE110-NYUGAIKBN   PIC X(01).
      *���ԣɣ�
                   07  RECE110-PTID        PIC 9(10).
      *�쥻�ץȿ��Ų�
                   07  RECE110-RECEKA      PIC X(02).
      *����ɣ�
                   07  RECE110-HKNID       PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE110-TEKSTYMD    PIC X(08). 
      *����̾�ե饰
               05  RECE110-SYUBYOFLG       PIC X(01).
      *���ų�����
               05  RECE110-SRYYMD          PIC X(08).
      *ž����ʬ
               05  RECE110-TENKIKBN        PIC X(01).
      *ž����
               05  RECE110-TENKIYMD        PIC X(08).
      *Ϣ�ֹ�
               05  RECE110-RENNUM          PIC 9(02).
      *��̾
               05  RECE110-BYOMEI          PIC X(160).
      *��̾ʸ����
           03  RECE110-BYOMEIMOJI        PIC 9(03).
