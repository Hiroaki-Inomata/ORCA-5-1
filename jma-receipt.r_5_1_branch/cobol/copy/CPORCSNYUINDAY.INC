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
      *    ������̤�����������ѥ�᥿��COPY (CPORCSNYUINDAY.INC)
      *                                ���ѥץ������ (ORORCSNYUINDAY)
      *    �������
      *      ���ŵ����ֹ�
      *      ���ԣɣ�
      *      ����ǯ��
      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSNYUINDAY-AREA.
      *
      *���ŵ����ֹ�
           03  LNK-NYUINDAY-HOSPNUM           PIC 9(2).
      *���ԣɣ�
           03  LNK-NYUINDAY-PTID              PIC  9(10).
      *����ǯ��
           03  LNK-NYUINDAY-SANTEIYM          PIC  X(06).
      *�ֵѹ���
           03  LNK-NYUINDAY-HENKYAKU-AREA.
      *������ʬ
             05  LNK-NYUINDAY-TBL.
      *  �ʶ����оݳ��������������������̤��������
               07  LNK-NYUINDAY-KBN           PIC  X(01)  OCCURS  31.
      *
      *û���ں߼�Ѵ������������ϣ���������ʬ
             05  LNK-NYUINDAY-TANKI2-TBL.
      *  �ʶ����оݳ�
      *    ������û���ں߼�Ѵ������������ϣ��ˡ��������ܴޤ�ˤ������о���
               07  LNK-NYUINDAY-TANKI-KBN    PIC  X(01)  OCCURS  31.
      *
      *  �ʶ����оݳ�
      *    ������û���ں߼�Ѵ������������ϣ��ˤ�������̤������
      *      ����û���ں߼�Ѵ������������ϣ��ˡ��������ܤ�������̤��������
               07  LNK-NYUINDAY-TANKI2-KBN    PIC  X(01)  OCCURS  31.
      *
      *ʿ������ǯ������ɲäˤʤä�û���ں߼�Ѵ�������������ʬ
      *  �ʶ����оݳ�
      *    ������û���ں߼�Ѵ���������������̤������
      *      ����û���ں߼�Ѵ����������������ܤ�������̤��������
               07  LNK-NYUINDAY-TANKI3-KBN    PIC  X(01)  OCCURS  31.
      *
      *�ߵ����������������ʬ
             05  LNK-NYUINDAY-KYUKYU-TBL.
      *  �ʶ���̤���� �����������
               07  LNK-NYUINDAY-KYUKYU-KBN    PIC  X(01)  OCCURS  31.
      *
      *Ʊ��������Ƚ����
             05  LNK-NYUINDAY-DOUJITSU-TBL.
      *        Ʊ����������ʬ�ʶ���Ʊ���������ʳ� ������Ʊ����������
               07  LNK-NYUINDAY-DOUJITSU-KBN  PIC  X(01)  OCCURS  31.
      *        �ౡ���������ݸ���ư��ʬ
      *        �ʣ������ݤ�����ݡ��������ݤ�����ݰʳ���
      *          �������ݰʳ�������ݡ��������ݰʳ�������ݰʳ���
               07  LNK-NYUINDAY-DOUJITSU-HKNHENKBN PIC X(01) OCCURS 31.
      *
      *        Ʊ��������������ʬ�ˤ��ݸ��ȹ礻
               07  LNK-NYUINDAY-DOUJITSU-HKNCOMBI PIC 9(04) OCCURS  31.
      *        Ʊ��������������ʬ�ˤ��ݸ�����
      *        �ʣ������ݡ��������ݡ�����������𡡣���ϫ�ҡ���������
      *          �����������������񡡣���������
               07  LNK-NYUINDAY-DOUJITSU-HKNSYU   PIC X(01) OCCURS  31.
      *        Ʊ��������������ʬ�ˤο������ϻ��ֶ�ʬ
      *       �ʣ������ʤ�������ī���顿�����뤫�顿����ͼ�����
               07  LNK-NYUINDAY-DOUJITSU-SKJKBN PIC X(01) OCCURS  31.
      *
      *        Ʊ�����������ౡʬ�ˤ��ݸ��ȹ礻
               07  LNK-NYUINDAY-DOUJITSU-T-HKNCOMBI PIC 9(04) OCCURS 31.
      *        Ʊ�����������ౡʬ�ˤ��ݸ�����
      *        �ʣ������ݡ��������ݡ�����������𡡣���ϫ�ҡ���������
      *          �����������������񡡣���������
               07  LNK-NYUINDAY-DOUJITSU-T-HKNSYU  PIC X(01) OCCURS  31.
      *        Ʊ�����������ౡʬ�ˤο�����λ���ֶ�ʬ
      *       �ʣ������ʤ�������ī�ޤǡ�������ޤǡ�����ͼ�ޤǡ�
               07  LNK-NYUINDAY-DOUJITSU-T-SKJKBN PIC X(01) OCCURS  31.
      *
      *���������ɡʣ�������ǯ��顼���������Ը������顼
      *            ����������פʤ�������������ʬ����ʤ���
           03  LNK-NYUINDAY-RC                PIC S9(9) BINARY.
      *