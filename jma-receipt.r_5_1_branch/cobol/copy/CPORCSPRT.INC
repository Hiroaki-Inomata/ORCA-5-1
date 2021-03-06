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
000100************************************************************
000200* �����ģ����極�֥ѥ�᥿    CPORCSPRT.INC                *
000300************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    02/10/10  �����ȶ�ʬ�ɲ�
      *  01.00.02    NACL-ƣ��    03/03/03  �����ѥե饰�ɲ�
      *  01.00.03    NACL-ƣ��    03/03/10  ü���ɣĤ򣶣����ѹ�
      ************************************************************
       01  ORCSPRTAREA.
      *    ������ʬ                              
           03  SPRT-MODE           PIC X(03).
      *    ����������     
           03  SPRT-RETURN         PIC S9(03).
      *    Ϣ��
           03  SPRT-RENNUM         PIC 9(04).
      *    �ˣţ�
           03  SPRT-TBL-KEY        PIC X(08).
      *    ���롼��
           03  SPRT-TBL-GROUP      PIC X(14).
      *    UUID
           03  SPRT-TBL-UUID       PIC X(36).
      *    ��̳������
           03  SPRT-GYOUMU-CD      PIC 9(03).
      *    ����ǯ��
           03  SPRT-SRYYM          PIC X(06).
      *    ����ǯ����
           03  SPRT-SKYYMD         PIC X(08).
      *    ������ɣ�
           03  SPRT-SHELLID        PIC X(20).
      *    ���ԣɣ�
           03  SPRT-PTID           PIC 9(10).
      *    ������Ϣ��
           03  SPRT-SHORI-RENNUM   PIC 9(04).
      *    ������Ϣ��
           03  SPRT-PRIORITY       PIC 9(04).
      *    Ģɼ�ɣ�
           03  SPRT-PRTID          PIC X(20).
      *    Ģɼ�����ȥ�
           03  SPRT-TITLE          PIC X(100).
      *    Ģɼ�ǡ���
           03  SPRT-PRTDATA        PIC X(20000).
      *    ü���ɣ�
           03  SPRT-TERMID         PIC X(64).
      *    ���ڥ졼���ɣ�
           03  SPRT-OPID           PIC X(16).
      *    �ץ��̾
           03  SPRT-PRTNM          PIC X(16).
      *    �����ȶ�ʬ
           03  SPRT-SITEKBN        PIC X(01).
      *    �����ѥե饰	
           03  SPRT-PRTFLG         PIC X(01).
      *    �ǡ����μ���
           03  SPRT-DATA-TYPE      PIC X(16).
      *    PDF(UUID)
           03  SPRT-PDF            PIC X(36).
      *    API-ID
           03  SPRT-API-ID         PIC X(20).
