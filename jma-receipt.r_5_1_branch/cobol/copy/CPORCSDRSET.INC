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
      *    �ɥ������Խ��ѥ�᥿����COPY (CPORCSDRSET.INC)
      *                            ���ѥץ������ 
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.04.00    NACL-¿��Ǽ  09/03/05  ����ô�����б��ʤ��ɲ�
      ************************************************************
       01  ORCSDRSETAREA.
      *
      *����
           03  ORCSDR-IN-AREA.
      *������ʬ�ʣ����Խ������������å���
               05  ORCSDR-KBN                  PIC  X(01).
      *����ǯ����
               05  ORCSDR-SYSYMD               PIC  X(08).
      *���Ųʡʲ��̤Υɥ������Խ��λ���
               05  ORCSDR-SRYKA                PIC  X(02).
      *������ʬ���ʣ�������ô����ʤ���
               05  ORCSDR-KBN2                 PIC  X(01).
      *
      *�ɥ������ʣɡݣϡ�
           03  ORCSDR-IN-DRCD-G.
      *        �ɥ����������ɡʣɣΡ�
               05  ORCSDR-IN-DRCD              PIC  X(04).
               05  ORCSDR-IN-DRCD-H            PIC  X(01).
      *        �ɥ�����̾�ʣϣգԡ�
               05  ORCSDR-OUT-DRCDMEI          PIC  X(24).
      *    �ơ��֥����
           03  ORCSDR-DRCD-IDX                 PIC  9(04).
      *����
           03  ORCSDR-OUT-AREA.
      *        �ơ��֥�
               05  ORCSDR-DRCD-TBL-G.
                   07  ORCSDR-DRCD-TBL         OCCURS   99.
      *                �ɥ�����������
                       09  ORCSDR-DRCD             PIC  X(04).
      *
                       09  ORCSDR-DRCD-H           PIC  X(01).
      *                �ɥ�����̾
                       09  ORCSDR-DRCDMEI          PIC  X(24).
      *        �ơ��֥�ͣ���
               05  ORCSDR-DRCD-MAX             PIC  9(04).
      *���顼�����ɡʣ����ɥ����������ɤʤ���
           03  ORCSDR-ERRCD                    PIC 9(01).
      *