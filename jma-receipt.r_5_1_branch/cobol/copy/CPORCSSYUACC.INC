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
      *    ����󥢥������ѥѥ�᥿      COPY (CPORCSSYUACC.INC)
      *                                  ���ѥץ������ (ORCSSYUACC)
      *    �������
      *      ������ʬ
      *      ����ǯ��
      *      ������ʬ
      *      ���ԣɣ�
      *      ���ϲ�
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     kohid �ɲ�
      *  04.06.02    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *                                     receipt_kbn �ɲ� 
      ************************************************************
      *
           03  LNK-SYUACC-IN.
      *        ������ʬ
      *        1:����ξ������
               05  LNK-SYUACC-I-KBN       PIC X(02).
      *        ����ǯ��
               05  LNK-SYUACC-I-SRYYM     PIC 9(06).
      *        ������ʬ
               05  LNK-SYUACC-I-NYUGAIKBN PIC X(01).
      *        ���ԣɣ�
               05  LNK-SYUACC-I-PTID      PIC 9(10).
      *
           03  LNK-SYUACC-IO.
      *        ���Ų�(�����Ͼ���)
               05  LNK-SYUACC-IO-H-SRYKA   PIC X(02).
      *        �쥻�ץȾ���
               05  LNK-SYUACC-IO-OCC               OCCURS  100.
      *            ������ʬ
                   07  LNK-SYUACC-IO-NYUGAIKBN
                                       PIC X(01).
      *            ������ʧ����
                   07  LNK-SYUACC-IO-TEISYUTUSAKI
                                       PIC 9(01).
      *            �쥻�ץȼ���
                   07  LNK-SYUACC-IO-RECESYUBETU
                                       PIC X(04).
      *            �ݸ����ֹ�
                   07  LNK-SYUACC-IO-HKNJANUM
                                       PIC X(08).
      *            �����ʬ
                   07  LNK-SYUACC-IO-HOJOKBN
                                       PIC X(01).
      *            ����ɣġʸ���ñ�ȤΤߡ�
                   07  LNK-SYUACC-IO-KOHID
                                       PIC 9(10).
      *            ʬ���ʬ
                   07  LNK-SYUACC-IO-RECEIPT-KBN
                                       PIC X(10).
      *            �ݸ��ֹ�ʼ��ݸ���
                   07  LNK-SYUACC-IO-HKNNUM
                                       PIC X(03).
      *            �����ƥ���
                   07  LNK-SYUACC-IO-SYUKA-AUTO
                                       PIC X(02).
      *            �����ϼ��
                   07  LNK-SYUACC-IO-SYUKA-INPUT
                                       PIC X(02).
      *            ����̾���
                   07  LNK-SYUACC-IO-SYUKA-AREA.
                       09  LNK-SYUACC-IO-SRYKATBL      OCCURS  50.
      *                    ���
                           11  LNK-SYUACC-IO-SRYKA
                                       PIC X(02).
      *                    �ݸ��ȹ礻�ֹ�
                           11  LNK-SYUACC-IO-HKNCOMBI-AREA.
                               13  LNK-SYUACC-IO-HKNCOMBI
                                       PIC 9(04)   OCCURS  10.
      *                    �����������
                           11  LNK-SYUACC-IO-TENSU
                                       PIC S9(09).
      *                    ���ǲ�����(������)
                           11  LNK-SYUACC-IO-KAISU    
                                       PIC 9(03).
      *                    �̻��������(����)
                           11  LNK-SYUACC-IO-NISSU
                                       PIC 9(03).
      *                    ��λ��
                           11  LNK-SYUACC-IO-EDDAY
                                       PIC 9(02).
      *
           03  LNK-SYUACC-IO2.
      *      ��ʥǡ�����Ʊ���쥤������
             05  LNK-SYUACC-IO2-OCC               OCCURS  100.
               07  LNK-IO2-SYU-KEY.
      *���ŵ��أɣ�
                 09  LNK-IO2-SYU-HOSPNUM       PIC 9(2).
      *������ʬ
                 09  LNK-IO2-SYU-NYUGAIKBN     PIC X(01).
      *���ԣɣ�
                 09  LNK-IO2-SYU-PTID          PIC 9(10).
      *��׶�ʬ
                 09  LNK-IO2-SYU-TEN-KBN       PIC X(01).
      *����ǯ��
                 09  LNK-IO2-SYU-SRYYM         PIC 9(06).
      *������ʧ����
                 09  LNK-IO2-SYU-TEISYUTUSAKI  PIC 9(01).
      *�ݸ����ֹ�
                 09  LNK-IO2-SYU-HKNJANUM      PIC X(08).
      *�����ʬ
                 09  LNK-IO2-SYU-HOJOKBN       PIC X(01).
      *����ɣġʸ���ñ�ȤΤߡ�
                 09  LNK-IO2-SYU-KOHID         PIC 9(10).
      *ʬ���ʬ
                 09  LNK-IO2-SYU-RECEIPT-KBN   PIC X(10).
      *�쥻�ץȼ���
                 09  LNK-IO2-SYU-RECESYUBETU   PIC X(04).
      *���Ų�
                 09  LNK-IO2-SYU-SRYKA         PIC X(02).
      *�ݸ��ֹ�ʼ��ݸ���
               07  LNK-IO2-SYU-HKNNUM          PIC X(03).
      *���Ųʡ����ϡ�
               07  LNK-IO2-SYU-SRYKA-INPUT     PIC X(02).
      *���Ųʡʥ����ƥ��
               07  LNK-IO2-SYU-SRYKA-AUTO      PIC X(02).
      *�ݸ��ȹ礻�ֹ�
               07  LNK-IO2-SYU-HKNCOMBI-AREA.
                 09  LNK-IO2-SYU-HKNCOMBI      PIC 9(04)
                                               OCCURS  10.
      *�����������
               07  LNK-IO2-SYU-TENSU           PIC S9(09).
      *���ǲ�����(������)
               07  LNK-IO2-SYU-KAISU           PIC 9(03).
      *�̻��������(����)
               07  LNK-IO2-SYU-NISSU           PIC 9(03).
      *��λ��
               07  LNK-IO2-SYU-EDDAY           PIC 9(02).
      *̤����쥻�ǻȤ�ͽ��
               07  LNK-IO2-SYU-MISEIKYU-FLG    PIC X(02).
      *����ե饰
               07  LNK-IO2-SYU-DLT-FLG         PIC X(01).
      *ü���ɣ�
               07  LNK-IO2-SYU-TERMID          PIC X(16).
      *���ڡ��졼���ɣ�
               07  LNK-IO2-SYU-OPID            PIC X(16).
      *����ǯ����
               07  LNK-IO2-SYU-CREYMD          PIC X(08).
      *����ǯ����
               07  LNK-IO2-SYU-UPYMD           PIC X(08).
      *��������
               07  LNK-IO2-SYU-UPHMS           PIC X(06).
      *    ����������
           03  LNK-SYUACC-RC           PIC S9(9) BINARY.
