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
      *-------------------------------------------------*
      *    ���ղ������ϥ����å��ѥ�᥿                 *
      *                         COPY  (CPORCSGDAY.INC)  *
      *
      *     SPTID-RC    : ZERO �����ｪλ��
      *                   10    (���ե��顼��
      *                   99    (�ѥ�᥿���顼��
      *-------------------------------------------------*
       01  ORCSGDAYAREA.
      *��������
           03  SGDAY-INDAY              PIC X(10).
      *���ϥ����סʣ���ǯ�����ǯ���������������פʤ���
           03  SGDAY-INTYPE             PIC X(01).
      *�����ƥ�����
           03  SGDAY-SYSYMD.
               05  SGDAY-SYSYY          PIC X(04).
               05  SGDAY-SYSMM          PIC X(02).
               05  SGDAY-SYSDD          PIC X(02).
      *��ά��ʬ�ʣ��������ƥ����գ���Σ��������Ϥ���ǯ
      *          ���������ƥ����դβ������
           03  SGDAY-SYSKBN             PIC X(01).
      *����
           03  SGDAY-OT-AREA.
      *        ���������ա�HZ9.Z9.Z9)
               05  SGDAY-OTDAY          PIC X(10).
      *        ��������
               05  SGDAY-SDAY           PIC X(08).
      *        ��������
               05  SGDAY-WDAY           PIC X(07).
      *
      *����������
           03  SGDAY-RC                 PIC S9(9)   BINARY.
      *