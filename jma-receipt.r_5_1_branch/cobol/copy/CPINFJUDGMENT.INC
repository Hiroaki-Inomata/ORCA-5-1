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
      *  ������Ƚ��                                (CPINFJUDGMENT.INC)*
      *****************************************************************
      *  ��������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
      *01  INFJUDGMENT-REC.
           03  INFJUD-KEY.
      *�ɣ�
             05  INFJUD-ID             PIC 9(05).
      *�¸��ζ�ʬ
           03  INFJUD-DISEASE          PIC 9(02).
      *Ƚ�ꥳ����
           03  INFJUD-CODE             PIC X(09).
      *�����ɶ�ʬ
           03  INFJUD-CODE-KBN         PIC 9(01).
      *���Ŷ�ʬ
           03  INFJUD-SRYKBN           PIC X(02).
      *����ǯ����
           03  INFJUD-CREYMD           PIC X(08).
      *����ǯ����
           03  INFJUD-UPYMD            PIC X(08).
      *��������
           03  INFJUD-UPHMS            PIC X(06).