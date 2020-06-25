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
      *    ����������������ּ����ѥ�᥿����COPY (CPORCSTOKNYUIN2.INC)
      *                                ���ѥץ������ (ORCSTOKNYUIN2)
      *    �������
      *      ���ŵ��أɣ�
      *      ���ԣɣ�
      *      ���곫����
      *      ����������������
      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSTOKNYUIN2-AREA.
      *
      *���ŵ��أɣ�
           03  LNK-TOKNYUIN2-HOSPNUM                 PIC  9(02).
      *���ԣɣ�
           03  LNK-TOKNYUIN2-PTID                    PIC  9(10).
      *��ư��ʬ�ʣ���������ʬ��׺���������������ʬ��׺�����
           03  LNK-TOKNYUIN2-IDOKBN                  PIC  X(01).
      *������
           03  LNK-TOKNYUIN2-NYUINYMD                PIC  X(08).
      *���곫����
           03  LNK-TOKNYUIN2-SANTEIYMD               PIC  X(08).
      *����������������
           03  LNK-TOKNYUIN2-SET-SRYCD               PIC  X(09).
      *�����������̻�������STUSAN-NISSU08�򥻥åȡ�
           03  LNK-TOKNYUIN2-NISSU08                 PIC  9(08).
      *������������ʬ
           03  LNK-TOKNYUIN2-TOKUTEI-NYUIN           PIC X(02).
      *���̽����ե饰�ΰ�
      *    ����ʿ������ǯ��������˰��������������Ŵ��������������
      *        ��������򣹣����Ȥ���
      *    ����������������¾�������������������������������Ķ����
      *        ���ʹߤ�������פ�������ʤ��ʰ�ư�����Ǻ�������١�
      *    �����飵��̤����
           03  LNK-TOKNYUIN2-TOKUBETSU-FLG           PIC X(01)
                                                     OCCURS  5.
      *����ֹ�����������Ͽ���줿����������Ͽʬ�ˤĤ��������������̻�
      *          �о��������������٤˥��åȡ�
           03  LNK-TOKNYUIN2-SHONUM                  PIC 9(03).
      *�ֵѹ���
           03  LNK-TOKNYUIN2-HENKYAKU-AREA.
      *���������������ɻ�������ֵ��ΰ�
             05  LNK-TOKNYUIN2-HENKYAKU-OCC          OCCURS  5.
      *����������������
                 07  LNK-TOKNYUIN2-SRYCD             PIC  X(09).
      *��������������
                 07  LNK-TOKNYUIN2-STYMD             PIC  X(08).
      *����������λ��
                 07  LNK-TOKNYUIN2-EDYMD             PIC  X(08).
      *����������׺����ե饰������������������¸��������׺�����
      *    �ʶ��򡧺������ʤ����������������
             05  LNK-TOKNYUIN2-IPN-KAIKEI-FLG        PIC  X(01).
      *����������׺���������
             05  LNK-TOKNYUIN2-IPN-KAIKEI-YMD        PIC  X(08).
      *���������ɡʣ��������������ֵѤ��ꡡ�����������������Ķ���ֵѤʤ�
      *            ���������������оݤʤ���
           03  LNK-TOKNYUIN2-RC                      PIC S9(9) BINARY.
      *