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
      *    ������Ź԰�Ÿ���������ѥ�᥿��COPY (CPORCSCKTWKSRY.INC)
      *                           ���ѥץ������ (ORCGKT01)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSCKTWKSRYAREA.
      *
      *��ʬ�ʣ���Ÿ�������������ʣģ¡ˡ����������ʥѥ�᥿��
           03  ORCSCWKSRY-KBN              PIC  X(01).
      *�����У�
           03  ORCSCWKSRY-PG               PIC  X(08).
      *
      *������ʬ�ʣ�������Ÿ�����������ӽ�λŸ���������ģ�Ÿ����
           03  ORCSCWKSRY-SYORIFLG         PIC  9(01).
      *Ÿ����
           03  ORCSCWKSRY-DAY              PIC  9(02).
      *�ɲó��ϰ���
           03  ORCSCWKSRY-STRIDX           PIC  9(04).
      *�ԥ����С����顼
           03  ORCSCWKSRY-ERRCD            PIC  X(04).
      *���顼
           03  ORCSCWKSRY-ERRCD2           PIC  X(04).
      *
      *�ģº����Τ�����ʼ�ư����ʬ��
      *    ��ư�û��ơ��֥�
           03  ORCSCWKSRY-KASAN-AREA.
               05  ORCSCWKSRY-KASAN-TBL    OCCURS   60.
                   07  ORCSCWKSRY-KSN-HKNCOMBI    PIC X(04).
                   07  ORCSCWKSRY-KSN-SRYKA       PIC X(02).
                   07  ORCSCWKSRY-KSN-SRYSYUKBN   PIC X(03).
                   07  ORCSCWKSRY-KSN-SRYKBN      PIC X(02).
                   07  ORCSCWKSRY-KSN-SRYCD       PIC X(09).
                   07  ORCSCWKSRY-KSN-ATAI-G.
                       09  ORCSCWKSRY-KSN-ATAI    PIC X(08)
                                                  OCCURS  4.
                   07  ORCSCWKSRY-KSN-TENSU       PIC 9(08).
                   07  ORCSCWKSRY-KSN-SURYO       PIC 9(05)V9(05).
                   07  ORCSCWKSRY-KSN-ZAINUM      PIC 9(08).
                   07  ORCSCWKSRY-KSN-ZAIEND      PIC X(01).
                   07  ORCSCWKSRY-KSN-INPUTCD     PIC X(22).
                   07  ORCSCWKSRY-KSN-AUTOKBN     PIC X(01).
      *            ����
                   07  ORCSCWKSRY-KSN-HKTKBN      PIC 9(01).
               05  ORCSCWKSRY-KSN-IDX             PIC 9(04).