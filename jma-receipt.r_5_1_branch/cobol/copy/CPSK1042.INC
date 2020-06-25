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
      *         �����ƥ�����ޥ���     (CPSK1042.INC)   *
      *         �ʣ���������                            *
      *         ��̾��ǽ����                            *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  ver.2.6.0
      *  00.00.01    NACL-ƣ��    05/04/20  �ܹ���̾�ٹ��å�����ɽ��
      *                                     ̵ͭ��ʬ
      *                                     �ѻ�ǯ�����ٹ��å�����ɽ��
      *                                     ̵ͭ��ʬ
      *                                     ���Ųʽ��ɽ����ʬ
      *                                     ɽ���оݴ��ֽ��ɽ����ʬ
      *                                     Ʊ����̾�ٹ��å�����ɽ��
      *                                     ̵ͭ��ʬ
      *                                     ���ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/06/16  �����ե饰�����б�
      *                                     �����ե饰�����ʬ���ɲ�
      *
      *  04.08.01    NACL-ƣ��    16/01/20  ž����ʬ�λ�˴�ѹ����δ���
      *                                     ���󹹿��б�
      *                                     ��˴��ʬ������ǧ��å�����ɽ��
      *                                     ̵ͭ��ʬ���ɲ�
      ************************************************************
       01  SYS-1042-REC.
           03  SYS-1042-KEY.
               05  SYS-1042-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1042-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1042-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1042-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1042-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-1042-TBL.
      *����̾��ǧ��å�����ɽ��̵ͭ��ʬ
               05  SYS-1042-MSG1-DSPKBN PIC  X(01).
      *�ܹ���̾�ٹ��å�����ɽ��̵ͭ��ʬ
               05  SYS-1042-MSG2-DSPKBN PIC  X(01).
      *�ѻ�ǯ�����ٹ��å�����ɽ��̵ͭ��ʬ
               05  SYS-1042-MSG3-DSPKBN PIC  X(01).
      *Ʊ����̾�ٹ��å�����ɽ��̵ͭ��ʬ
               05  SYS-1042-MSG4-DSPKBN PIC  X(01).
      *���Ųʽ��ɽ����ʬ
               05  SYS-1042-SRYKA-DSPKBN
                                        PIC  X(01).
      *ɽ���оݴ��ֽ��ɽ����ʬ
               05  SYS-1042-SYOKBN-DSPKBN
                                        PIC  X(01).
      *�����ե饰�����ʬ
               05  SYS-1042-UTAGAIFLG-KBN
                                        PIC  X(01).
      *ͽ��
               05  SYS-1042-YOBI1
                                        PIC  X(09).
      *�����Ⱦɾ��ܵ���ʬ�������
               05  SYS-1042-SJKBN-SET
                                        PIC  X(02).
      *��˴��ʬ������ǧ��å�����ɽ��̵ͭ��ʬ
               05  SYS-1042-MSG5-DSPKBN PIC  X(01).
      *ͽ��
      ******** 05                       PIC  X(494).
               05  SYS-1042-YOBI        PIC  X(481).
      *ü���ɣ�
           03  SYS-1042-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1042-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1042-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1042-UPYMD           PIC  X(08).
      *��������
           03  SYS-1042-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1042-UPD-KEY.
               05  SYS-1042-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1042-UPD-KANRICD     PIC X(04).
               05  SYS-1042-UPD-KBNCD       PIC X(08).
               05  SYS-1042-UPD-STYUKYMD    PIC X(08).
               05  SYS-1042-UPD-EDYUKYMD    PIC X(08).