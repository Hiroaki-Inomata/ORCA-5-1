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
      *    ������������Խ�����COPY (CPORCSS006.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������        ����      ����
      ************************************************************
       01  SS006-AREA.
           03  SS006-IN.
      *������ֳ��ϳ�����
               05  SS006-SKYSTYMD          PIC X(08).
      *������ֳ��Ͻ�λ��
               05  SS006-SKYEDYMD          PIC X(08).
      *�׻���ʬ(1:���ֽ�λ�����ౡ��/2:���ֽ�λ�����ౡ��)
               05  SS006-SKYKBN            PIC X(01).
           03  SS006-OT.
      *���ᳫ����Ʊ���������ե饰
               05  SS006-DOJITUFLG-ST      PIC  X(01).
      *����������������ʬ�ʣ������ʤ�������ī���顢�����뤫�顢����ͼ�����
               05  SS006-TIMEKBN-ST        PIC  X(01).
      *��������ƻ�û��������
               05  SS006-SYOKUDO-OK        PIC  9(01).
      *���Ὢλ��Ʊ���������ե饰
               05  SS006-DOJITUFLG-ED      PIC  X(01).
      *��λ������������ʬ�ʣ������ʤ�������ī�ޤǡ�������ޤǡ�����ͼ�ޤǡ�
               05  SS006-TIMEKBN-ED        PIC  X(01).