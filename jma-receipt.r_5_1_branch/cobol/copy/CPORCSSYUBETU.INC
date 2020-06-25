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
      *    �쥻�ץȼ����Խ��ѥ�᥿      COPY (CPORCSSYUBETU.INC)
      *                                  ���ѥץ������ (ORCSSYUBETU)
      *    �������
      *      ���ŵ��أɣ�
      *      ���ֹ�
      *      ���ԣɣ�
      *      ����ǯ��
      *      ������ʬ
      *      ǯ��
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *
      *  04.01.01    NACL-ƣ��    07/01/21  ���������ô���ֹ��ѹ��б�
      *                                     �����ݤθ���ñ�ȤΤߡ�
      *                                     Ŭ�ѳ���ǯ�������ɲ�
      *
      *  04.03.01    NACL-ƣ��    08/05/07  ���񣴼�ޤǤ�ʻ�Ѥε�Ͽ�б�
      *                                     �쥻�ץȼ��̡ʻ�쥻�ץȡˤ��ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     kohid �ɲ�
      *  04.06.02    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *                                     receipt_kbn �ɲ� 
      ************************************************************
      *
       01  ORCSSYUBETUAREA.
      *
           03  LNK-SYUBETU-IN.
      *        ���ŵ��أɣ�
               05  LNK-SYUBETU-HOSPNUM     PIC 9(2).
      *        ���ֹ�
               05  LNK-SYUBETU-PREFNUM     PIC 9(02).
      *        ����ǯ��
               05  LNK-SYUBETU-SRYYM       PIC 9(06).
      *        ������ʬ
               05  LNK-SYUBETU-NYUGAIKBN   PIC X(01).
      *        ���ԣɣ�
               05  LNK-SYUBETU-PTID        PIC 9(10).
      *        ��ǯ����
               05  LNK-SYUBETU-BIRTHDAY    PIC X(08).
      *
           03  LNK-SYUBETU-OT.
      *        �쥻�ץȾ���
               05  LNK-SYUBETU-OCC         OCCURS  100.
      *            ������ʧ����
                   07  LNK-SYUBETU-TEISYUTUSAKI
                                       PIC 9(01).
      *            �쥻�ץȼ���
                   07  LNK-SYUBETU-RECESYUBETU
                                       PIC X(04).
      *            �쥻�ץȼ��̡ʻ�쥻�ץȡ�
                   07  LNK-SYUBETU-RECESYUBETU-Y
                                       PIC 9(04).
      *            �ݸ����ֹ�
                   07  LNK-SYUBETU-HKNJANUM
                                       PIC X(08).
      *            �����ʬ
                   07  LNK-SYUBETU-HOJOKBN
                                       PIC X(01).
      *            ����ɣġʸ���ñ�ȤΤߡ�
                   07  LNK-SYUBETU-KOHID
                                       PIC 9(10).
      *            Ŭ�ѳ���ǯ����
                   07  LNK-SYUBETU-TEKSTYMD
                                       PIC X(08).
      *            ʬ���ʬ
                   07  LNK-SYUBETU-RECEIPT-KBN
                                       PIC X(10).
      *            �ݸ��ֹ�ʼ��ݸ���
                   07  LNK-SYUBETU-HKNNUM
                                       PIC X(03).
      *            �ݸ��ȹ礻�ֹ�
                   07  LNK-SYUBETU-HKNCOMBI-AREA.
                       09  LNK-SYUBETU-HKNCOMBI
                                       PIC 9(04)   OCCURS  10.
      *
      *    ����������  ����������ܥ��顼
      *                ��������ǯ��顼��ʿ������ǯ���������
      *                ����ǯ��������顼
      *                �����оݥ쥻�ץȤʤ�
           03  LNK-SYUBETU-RC              PIC S9(9) BINARY.