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
      *         ���������ܥӥ塼  COPY (CPMATERIAL-H.INC)      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  MATERIAL-H-REC.
      *    �쥳���ɴ����ֹ�
           03  MATERH-KANRINO          PIC 9(08).
      *    �ʣ��Υ�����
           03  MATERH-JANCD            PIC X(13).
      *    ����̾
           03  MATERH-SNAME            PIC X(200).
      *    ����
           03  MATERH-KIKAKU           PIC X(120).
      *    �����ॳ����
           03  MATERH-SRYCD            PIC  X(09).
      *    ��¤����ȼ�̾
           03  MATERH-COMPANY1         PIC  X(100).
      *    ���������丵�ȼ�̾
           03  MATERH-COMPANY2         PIC  X(100).
      *    ź��ʸ��������������
           03  MATERH-DOCKANRICD       PIC  X(30).
      *    ��ǽ��ʬ�ֹ�
           03  MATERH-KINOKBNNO        PIC  X(30).
      *    �ѹ�ǯ����
           03  MATERH-CHGYMD           PIC  X(08).
      *
           03  MATERH-CREYMD           PIC  X(08).
           03  MATERH-UPYMD            PIC  X(08).
           03  MATERH-UPHMS            PIC  X(06).