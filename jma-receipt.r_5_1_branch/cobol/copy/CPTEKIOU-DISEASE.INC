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
      **************************************************************
      *         Ŭ����̾����̾�ޥ���  COPY (CPTEKIOU-DISEASE.INC)  *
      **************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  TEKIOU-DISEASE-REC.
           03  TEKDIS-KEY.
      *��ǽ������
               05  TEKDIS-KONOCD       PIC X(05).
      *����̾
               05  TEKDIS-BYOMEI       PIC X(200).
      *���ֽ�����
           03  TEKDIS-BEFORE-BYOMEI    PIC X(100).
      *���ֽ�����
           03  TEKDIS-AFTER-BYOMEI     PIC X(100).
      *����̾������
           03  TEKDIS-BYOMEICD         PIC X(07).
      *���ֽ����쥳����
           03  TEKDIS-BEFORE-BYOMEICD  PIC X(04).
      *���ֽ����쥳����
           03  TEKDIS-AFTER-BYOMEICD   PIC X(04).
      *ICD
           03  TEKDIS-ICD              PIC X(05).
      *����ǯ��
           03  TEKDIS-KAITEIYM         PIC X(06).
      *�����ʬ
           03  TEKDIS-HOJYOKBN         PIC 9(02).
      *��ǽ̾���ֹ�
           03  TEKDIS-KONOMEINO        PIC 9(06).
      *
           03  TEKDIS-CREYMD           PIC X(08).
           03  TEKDIS-UPYMD            PIC X(08).
           03  TEKDIS-UPHMS            PIC X(06).
      *�����ѿ��Ź԰٥�����
           03  TEKDIS-SELSRYCD         PIC X(09).
      *��������̾
           03  TEKDIS-SELBYOMEI        PIC X(200).