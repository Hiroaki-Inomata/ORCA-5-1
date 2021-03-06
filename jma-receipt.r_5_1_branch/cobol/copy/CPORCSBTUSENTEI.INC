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
      *    ������������������ô����  COPY (CPORCSBTUSENTEI.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSBTUSENTEIAREA.
      *  ���ŵ��إ�����
           03  SBTUSENTEI-HOSPNUM          PIC 9(2).
      *  ���ԣɣ�
           03  SBTUSENTEI-PTID             PIC  9(10).
      *    ����ǯ��
           03  SBTUSENTEI-SRYYM            PIC  X(06).
      *
      *  �����ΰ�
      *    �̻�����
           03  SBTUSENTEI-OUT-AREA.
               05  SBTUSENTEI-DAY-AREA.
                   07  SBTUSENTEI-DAY-OCC  OCCURS  31.
      *                �����ֹ�
                       09  SBTUSENTEI-DAY-BTUNUM
                                           PIC  X(02).
      *                �������������ʬ
      *                1:��Ψ���� / 2:�������
                       09  SBTUSENTEI-DAY-FTNKBN
                                           PIC  X(01).
      *                �����Ƕ�ʬ
      *                0:�ʤ� / 1:����
                       09  SBTUSENTEI-DAY-TAXKBN
                                           PIC  X(01).
      *                ����ñ������Ψ�����ͭ����
                       09  SBTUSENTEI-DAY-TANKA
                                           PIC  9(02).
      *                ������ζ�ۡ���������ͭ����
                       09  SBTUSENTEI-DAY-KINGAKU
                                           PIC  9(04).
      *  �꥿���󥳡���
           03  SBTUSENTEI-RC               PIC 9(02).
