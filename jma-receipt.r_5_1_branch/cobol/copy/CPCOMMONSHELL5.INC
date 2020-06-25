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
      *****************************************************************
      *
      *    �ǡ��������å��ѥ��������ΰ�    CPCOMMONSHELL5.INC
      *            
      *      $1 SRYYM TERID SYSYMD
      *      $2 SYOKBN
      *         0:
      *      $3 RECEKBN
      *         0:
      *      $4 ���顼�ե�����̾
      *      $5 ����ѥ����ߥʥ�ɣ�
      *
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    �Ҥ��ʤ�     02/09/13  �ǥ��쥯�ȥ�򥵥֥ץ���
      *  01.00.02    �Ҥ��ʤ�     03/03/11  �����ߥʥ�ɣ��ѹ�
      *
      *****************************************************************
       01  SHELL-AREA.
           03  SHELL-NAME.
      *         05  FILLER             PIC  X(32) VALUE
      *                   "/usr/local/orca/scripts/allways/".
      *         05  SHELL-SHELL        PIC  X(11).
      *         05  FILLER             PIC  X(57) VALUE SPACE.
               05  SHELL-SHELL        PIC  X(43).
               05  FILLER             PIC  X(57) VALUE SPACE.
           03  SHELL-ARG1.
               05  SHELL-SRYYM        PIC  X(06).
      *         05  SHELL-TERMID       PIC  X(32).
               05  SHELL-TERMID       PIC  X(64).
               05  SHELL-SYSYMD       PIC  X(08). 
               05  SHELL-OPID         PIC  X(16).
               05  FILLER             PIC  X(06) VALUE SPACE.
           03  SHELL-ARG2.
               05  SHELL-SYOKBN       PIC  X(01) VALUE SPACE.
               05  FILLER             PIC  X(99) VALUE SPACE.
           03  SHELL-ARG3.
               05  SHELL-RECEKBN      PIC  X(01) VALUE SPACE.
               05  FILLER             PIC  X(99) VALUE SPACE.
           03  SHELL-ARG4.
               05  SHELL-ERRFILE      PIC  X(90) VALUE SPACE.
               05  FILLER             PIC  X(10) VALUE SPACE.
           03  SHELL-ARG5.
      *         05  SHELL-WK-TERMID    PIC  X(32) VALUE SPACE.
               05  SHELL-WK-TERMID    PIC  X(64) VALUE SPACE.
               05  FILLER             PIC  X(36) VALUE SPACE.