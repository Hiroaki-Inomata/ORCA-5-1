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
      *    �ե��������ģ����極�֥ѥ�᥿     CPORCSFILESV.INC *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSFILESVAREA.
      *    ������ʬ                              
           03  SFILESV-MODE            PIC X(01).
      *    ����������     
           03  SFILESV-RETURN          PIC S9(03).
      *    �ˣţ�
           03  SFILESV-TBL-KEY         PIC X(08).
      *
           03  SFILESV-OCC             OCCURS  100.
      *        ������ɣ�
               05  SFILESV-SHELLID     PIC X(20).
      *        ������Ϣ��
               05  SFILESV-SHORI-RENNUM
                                       PIC 9(03).
      *        Ϣ��
               05  SFILESV-RENNUM      PIC 9(03).
      *        ����ǯ��
               05  SFILESV-SRYYM       PIC X(06).
      *        ����ǯ����
               05  SFILESV-SKYYMD      PIC X(08).
      *        �����ȥ�
               05  SFILESV-TITLE       PIC X(100).
      *        ������ʧ����
               05  SFILESV-TEISYUTUSAKI
                                       PIC X(01).
      *        ������ʬ
               05  SFILESV-NYUGAIKBN   PIC X(01).
      *        ���
               05  SFILESV-CNT-ALL     PIC 9(07).
      *        ����Ǽ���
               05  SFILESV-FOR-FOLDER  PIC X(200).
      *        ���ե�����̾
               05  SFILESV-FOR-DATA    PIC X(200).
      *        ����¸���
               05  SFILESV-TO-FOLDER   PIC X(200).
      *        ��ե�����̾
               05  SFILESV-TO-DATA     PIC X(200).
      *        �����ɶ�ʬ
               05  SFILESV-CODE-TYPE   PIC X(01).
      *        �ãӣ��ѣģ�̵ͭ��ʬ
               05  SFILESV-DATA-TYPE   PIC X(01).
      *        ���顼������
               05  SFILESV-ERR-CODE    PIC X(01).
      *        ���顼��å�����
               05  SFILESV-ERR-MSG     PIC X(200).
      *        ������
               05  SFILESV-SYOYMD      PIC X(08).
      *        ������
               05  SFILESV-CREYMD      PIC X(08).
      *        ��������
               05  SFILESV-CREHMS      PIC X(06).