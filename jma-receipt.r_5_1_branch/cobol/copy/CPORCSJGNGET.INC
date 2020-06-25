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
      *    ������³ۼ����ѥѥ�᥿������ COPY (CPORCSJGNGET.INC)
      *                          ���ѥץ������  (ORCSJGNGET)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-����    06/03/20  ¾���ŵ�����������ô�߷��ɲ�
      *  01.00.02    NACL-����    09/07/10  ¾���ŵ�����������ô�߷�
      *                                     �Խ��ե饰�ɲ�
      *  04.07.01    NACL-����    11/09/01  �����ƥ��������������ǽ�ɲ��б�
      *                                     ����Ŭ�ѳ������ɲ�
      *                                     ��ƻ�ܸ��ֹ��ɲ�
      *                                     �оݶ�ʬ�ǥե��������
      *                                     �Խ��ե饰
      *                                     �оݶ�ʬ�ɲ�
      *  04.07.02    NACL-����    15/01/26  ʿ������ǯ��������б�
      *                                     ����ƻ�ܸ��ȼ������б���
      ************************************************************
       01  LNK-ORCSJGNGET-REC.
      *
      *���ŵ��أɣ�
           03  LNK-ORCSJGNGET-HOSPNUM       PIC 9(2).
      *������ʬ
           03  LNK-ORCSJGNGET-NYUGAIKBN     PIC  X(01).
      *���ԣɣ�
           03  LNK-ORCSJGNGET-PTID          PIC  9(10).
      *�����ݸ��ֹ�
           03  LNK-ORCSJGNGET-KOHHKNNUM     PIC  X(03).
      *����ɣ�
           03  LNK-ORCSJGNGET-KOHID         PIC  9(10).
      *���ŷ�
           03  LNK-ORCSJGNGET-SRYYM         PIC  X(06).
      *�������Ὢλ��
           03  LNK-ORCSJGNGET-SKYEDYMD      PIC  X(08).
      *����Ŭ�ѳ�����
           03  LNK-ORCSJGNGET-TEKSTYMD      PIC  X(08).
      *��ƻ�ܸ��ֹ�
           03  LNK-ORCSJGNGET-PREFNUM       PIC  X(02).
      *�оݶ�ʬ�ǥե��������
           03  LNK-ORCSJGNGET-SETKBN        PIC  9(01).
      *���¡���������Ŭ�Ѷ�ʬ�б��ե饰�ǥե��������
           03  LNK-ORCSJGNGET-SETKBN2       PIC  9(01).
      *�Խ��ե饰
      *���ʶ���ˡ���³����Ϥ��оݥ����å���Ԥ�ʤ�
      *���������������������������������оݶ�ʬ�Ⱦ�³ۤ��ֵ�
      *���ʣ����ˡ���³����Ϥ��оݥ����å���Ԥ�
      *���������������������񡡡��������оݶ�ʬ���ֵ�
      *����������������������ʳ����������顼���ֵ�
      *���ʣ����ˡ���³����Ϥ��оݥ����å���Ԥ�
      *���������������������񡡡��������оݶ�ʬ�Ⱦ�³ۤ��ֵ�
      *����������������������ʳ����������顼���ֵ�
           03  LNK-ORCSJGNGET-HENFLG        PIC  X(01).
      *
      *�ֵ���
      *
           03  LNK-ORCSJGNGET-OUT-AREA.
      *�����³�
             05  LNK-ORCSJGNGET-GAIJGNGAK   PIC  9(07).
      *������³�
             05  LNK-ORCSJGNGET-NYUJGNGAK   PIC  9(07).
      *¾���ŵ�����������ô�߷ס�024,079:�������ܾ�³ۡ�
             05  LNK-ORCSJGNGET-TAFTNRUIKEI PIC  9(07).
      *¾���ŵ�����������ô�߷��Խ��ե饰
             05  LNK-ORCSJGNGET-TAFTNFLG    PIC  9(01).
      *�оݶ�ʬ
      *���ʣ��ˡ���³����Ϥ��оݳ�
      *���ʣ��ˡ���³����Ϥ��оݡ���������ˡ������³ۡ�������³�
      *���ʣ��ˡ���³����Ϥ��оݡ���������ˡ�������³ۡ�¾������ô�߷�
      *���ʣ��ˡ���³����Ϥ��оݡ���������ʳ���
             05  LNK-ORCSJGNGET-TAISYOKBN   PIC  9(01).
      *���¡���������Ŭ�Ѷ�ʬ�б��ե饰
             05  LNK-ORCSJGNGET-TEKIYOFLG   PIC  9(01).
      *
      *����������
      *
           03  LNK-ORCSJGNGET-RC            PIC  9(02).
      *