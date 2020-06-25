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
      *    �ե����������ž��󥷥��ƥ���    CPORCSFILEMISSV.INC *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSFILEMISSVAREA.
      *    ������ʬ                              
           03  FLMISSV-MODE            PIC X(01).
      *    ����������     
           03  FLMISSV-RETURN          PIC S9(03).
      *    �ˣţ�
           03  FLMISSV-TBL-KEY         PIC X(08).
      *
           03  FLMISSV-OCC             OCCURS  100.
      *        Ϣ��
               05  FLMISSV-RENNUM      PIC 9(04).
      *        ���롼��
               05  FLMISSV-TBL-GROUP   PIC X(14).
      *        ������Ϣ��
               05  FLMISSV-SHORI-RENNUM
                                       PIC 9(04).
      *        ������ɣ�
               05  FLMISSV-SHELLID     PIC X(20).
      *        ����ǯ��
               05  FLMISSV-SRYYM       PIC X(06).
      *        ����ǯ����
               05  FLMISSV-SRYYMD      PIC X(08).
      *        ���
               05  FLMISSV-CNT         PIC 9(07).
      *        ������ʬ
               05  FLMISSV-NYUGAIKBN   PIC X(01).
      *        ����¸���
               05  FLMISSV-TO-FOLDER   PIC X(200).
      *        ��ե�����̾
               05  FLMISSV-TO-DATA     PIC X(200).
      *        �����ɶ�ʬ
               05  FLMISSV-CODE-TYPE   PIC X(01).
      *        ���ϥե饰
               05  FLMISSV-PUTFLG      PIC X(1).
      *        �����ȥ�
               05  FLMISSV-TITLE       PIC X(100).
      *        UUID
               05  FLMISSV-TBL-UUID    PIC X(36).
      *        ����ǯ����
               05  FLMISSV-CREYMD      PIC X(08).
      *        ����ǯ����
               05  FLMISSV-UPYMD       PIC X(08).
      *        ��������
               05  FLMISSV-UPHMS       PIC X(06).