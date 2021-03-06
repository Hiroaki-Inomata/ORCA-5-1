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
      * ������Ģ�ǡ������極�֥ѥ�᥿     CPORCSMNOTE.INC    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSMNOTEAREA.
      *    ������ʬ                              
           03  SMNOTE-MODE         PIC X(03).
      *    ����������     
           03  SMNOTE-RETURN       PIC S9(03).
      *    Ϣ��
           03  SMNOTE-RENNUM       PIC 9(04).
      *    �ˣţ�
           03  SMNOTE-TBL-KEY      PIC X(08).
      *    ���롼��
           03  SMNOTE-TBL-GROUP    PIC X(14).
      *    ����ǯ��
           03  SMNOTE-SRYYM        PIC X(06).
      *    ����ǯ����
           03  SMNOTE-SRYYMD       PIC X(08).
      *    ������ɣ�
           03  SMNOTE-SHELLID      PIC X(20).
      *    ���ԣɣ�
           03  SMNOTE-PTID         PIC 9(10).
      *    ������Ϣ��
           03  SMNOTE-SHORI-RENNUM PIC 9(04).
      *    �����ѥե饰	
           03  SMNOTE-PUTFLG       PIC X(01).
      *    ������ʬ
           03  SMNOTE-NYUGAIKBN    PIC X(01).
      *    ���Ǽ���
           03  SMNOTE-TO-FOLDER    PIC X(200).
      *    ��ե�����̾
           03  SMNOTE-TO-DATA      PIC X(200).
      *    �����ɶ�ʬ�ʶ��򡢣����ӡݣ��� �����ţգá�
           03  SMNOTE-CODE-TYPE    PIC X(01).
      *    Ģɼ�����ȥ�
           03  SMNOTE-TITLE        PIC X(100).
      *    �գգɣ�
           03  SMNOTE-TBL-UUID     PIC X(36).
      *    ��̳������
           03  SMNOTE-GYOUMU-CD    PIC 9(03).
      *    ���Ͼ�ꥳ����
      *    01:���Ź԰١������ǧ��
      *    02:���Ź԰١����ӽ�λ��
      *    03:���Ź԰١����������
           03  SMNOTE-LOCATION-CD  PIC X(02).
      *    ��¸��CSV�ե�����̾(PATH��)
           03  SMNOTE-CSVFILE      PIC X(255).
      *    �ɥ�����������
           03  SMNOTE-DRCD         PIC X(05).
      *    Ģɼ�ǡ���
           03  SMNOTE-CSVDATA      PIC X(20000).
      *    �ǡ����С������
           03  SMNOTE-DATA-VERSION PIC X(1).
