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
      *----------------------------------------------------------*
      *    �桼���ץ�����൯ư�ѥ�᥿      COPY (CPORCSUSERPG.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.00    NACL-¿��Ǽ  16/06/XX  ��ɼ�ֹ��ɲ��б�
      ************************************************************
      *
       01  ORCSUSERPGAREA.
      *
      *�����У�
           03  ORCSUSERPG-GMNPG            PIC X(08).
      *
      *���������ʣ����ɲá��������������������
      *�����ౡ��Ͽ��̳�϶�̳�ν�����ʬ��01:�������ˤȤʤ롣
           03  ORCSUSERPG-SYORI-FLG        PIC 9(02).
      *�����ƥ���������
           03  ORCSUSERPG-SYSYMD           PIC X(08).
      *������
           03  ORCSUSERPG-SRYYMD           PIC X(08).
      *���ԣɣ�
           03  ORCSUSERPG-PTID             PIC 9(10).
      *�����ֹ�
           03  ORCSUSERPG-PTNUM            PIC X(20).
      *���Ų�
           03  ORCSUSERPG-SRYKA            PIC X(02).
      *�ɥ�����
           03  ORCSUSERPG-DRCD             PIC X(05).
      *�ݸ��ȹ礻
           03  ORCSUSERPG-HKNCOMBI         PIC X(04).
      *������Ģɼ̾
           03  ORCSUSERPG-SYORINM          PIC X(30).
      *�¹�̾
           03  ORCSUSERPG-SHELL            PIC X(100).
      *�ץ��̾
           03  ORCSUSERPG-PRTNM            PIC X(16).
      *��ɼ�ֹ�(K03)
           03  ORCSUSERPG-DENPNUM          PIC 9(07).
      *��ݥ��ȥ�̾
           03  ORCSUSERPG-REPOS-NAME       PIC X(128).
      *