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
      *    �����Ѵ�      COPY (CPORCSKANACNV.INC)                *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-����     02.05.29  ��������
      *  04.08.01    NACL-����    15/12/21  �����Ѵ��б�
      ************************************************************
       01  KANACNV-LNK.
      *    �����ե饰�ʶ��򡧥����Ѵ������������Ѵ���
           03  KANACNV-SYORI           PIC  X(01).
           03  KANACNV-IN-STR          PIC  X(200).
      *    1:�Ҥ餬�ʤΤߡ�2:�������ʤΤߡ�3:����
           03  KANACNV-TYPE            PIC  9(01).
      *    1:�Ҥ餬�ʤΤߤξ��ĥ������ʤ��Ѵ���������
      *    2:�������ʤΤߤξ��ĥ������ʤ��Ѵ���������
      *    3:�����ξ��Ķ��������
           03  KANACNV-OT-STR          PIC  X(200).
      *    �꥿���󥳡���
      *    0:����
      *    1:Ⱦ�Ѻ���
           03  KANACNV-RC              PIC  9(01).