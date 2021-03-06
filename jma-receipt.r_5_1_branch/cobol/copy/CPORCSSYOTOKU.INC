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
      ************************************************************
      *    ���Խ��������Խ��ѥ�᥿
      *    COPY  (CPORCSSYOTOKU.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSSYOTOKUAREA.
      *������
      *    ���ŵ��أɣ�
           03  LNK-SYOTOKU-HOSPNUM         PIC 9(02).
      *    ���ԣɣ�
           03  LNK-SYOTOKU-PTID            PIC 9(10).
      *    ����ǯ��
           03  LNK-SYOTOKU-SRYYM           PIC X(06).
      *    ������ʬ�����å��ܡڿ����ѡ��ֵѹ����Խ�
           03  LNK-SYOTOKU-SKJCHKFLG       PIC X(01).
      *    �ڿ����ѡۿ�����ʬ����ǥå����ʥ쥻�ץ��ѡ˼����Ѥ��ݸ��Ȥ߹�碌
           03  LNK-SYOTOKU-HKNCOMBIINF.
               05  LNK-SYOTOKU-HKNCOMBI    PIC 9(04)   OCCURS  10.
      *�ֵѹ���
           03  LNK-SYOTOKU-HENKYAKU-AREA.
               05  LNK-SYOTOKU-HENKYAKU-TABLE     OCCURS  31.
      *���ݸ�����������ʬ
      *  �ʶ��򡧰��̡������㣱�������㣲�������㣱��Ϸ��ˡ�
                   07  LNK-SYOTOKU-SYUKBN         PIC  X(01).
      *���������ʬ
      *  �ʶ��򡧰��̡������㣱�������㣲��
                   07  LNK-SYOTOKU-CHIKBN         PIC  X(01).
      *������ʬ
      *  �ʶ��򡧰��̡������㣱�������㣲�������㣲�ʣ�����Ķ�ˡ������㣱��Ϸ��ˡ�
                   07  LNK-SYOTOKU-SKJKBN         PIC  X(01).
      *
      *�ڿ����ѡ۶����ض�ʬ
      *  �ʶ�������������������
                   07  LNK-SYOTOKU-KKSKBN         PIC  X(01).
      *�ڿ����ѡ�ǯ���ʬ
      *  �ʿ��������ݸ��Ȥ߹�碌��Ʊ��������ʬ��ޤ�ˤ�
      *    0000��������ϫ�ҡ������ա����������񡦼����ξ��϶����
      *  �ʶ����оݳ�����������ԡ�����������̤����
                   07  LNK-SYOTOKU-AGEKBN         PIC  X(01).
      *
      *�ڿ����ѡۿ�����ʬ����ǥå����ʥ쥻�ץ��ѡ�
               05  LNK-SYOTOKU-SKJIDX             PIC  9(02).
      *
      *���������ɡʣ�������ǯ��顼���������ԣɣĥ��顼��
      *          �ʣ����㣱���顼�����������㣲���顼������
           03  LNK-SYOTOKU-RC                     PIC  S9(9) BINARY.
      *
