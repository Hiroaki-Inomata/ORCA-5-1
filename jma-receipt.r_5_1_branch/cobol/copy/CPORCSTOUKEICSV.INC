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
      * ���ףãӣ����極�֥ѥ�᥿        CPORCSTOUKEICSV.INC    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSTOUKEICSVAREA.
      *    ������ʬ                              
           03  STOUKEICSV-MODE         PIC X(03).
      *    ����������     
           03  STOUKEICSV-RETURN       PIC S9(03).
      *    Ϣ��
           03  STOUKEICSV-RENNUM       PIC 9(04).
      *    �ˣţ�
           03  STOUKEICSV-TBL-KEY      PIC X(08).
      *    ���롼��
           03  STOUKEICSV-TBL-GROUP    PIC X(14).
      *    ����ǯ��
           03  STOUKEICSV-SRYYM        PIC X(06).
      *    ����ǯ����
           03  STOUKEICSV-SRYYMD       PIC X(08).
      *    ������ɣ�
           03  STOUKEICSV-SHELLID      PIC X(20).
      *    ���ԣɣ�
           03  STOUKEICSV-PTID         PIC 9(10).
      *    ������Ϣ��
           03  STOUKEICSV-SHORI-RENNUM PIC 9(04).
      *    �����ѥե饰	
           03  STOUKEICSV-PUTFLG       PIC X(01).
      *    ������ʬ
           03  STOUKEICSV-NYUGAIKBN    PIC X(01).
      *    ���Ǽ���
           03  STOUKEICSV-TO-FOLDER    PIC X(200).
      *    ��ե�����̾
           03  STOUKEICSV-TO-DATA      PIC X(200).
      *    �����ɶ�ʬ�ʶ��򡢣����ӡݣ��� �����ţգá�
           03  STOUKEICSV-CODE-TYPE    PIC X(01).
      *    Ģɼ�����ȥ�
           03  STOUKEICSV-TITLE        PIC X(100).
      *    Ģɼ�ǡ���
           03  STOUKEICSV-CSVDATA      PIC X(20000).
