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
      *         �����ƥ�����ޥ���     (CPSK1900.INC)            *
      *         �ʣ���������                                     *
      *         ���ŵ����Խ������Ģɼ�̻������                 *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-1900-REC.
           03  SYS-1900-KEY.
               05  SYS-1900-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1900-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1900-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1900-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1900-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-1900-TBL.
      *Ģɼ�̻���   1:���ݡ��쥻�ץȡ���������
      *             2:ϫ�ҡ��쥻�ץȡ���������
      *             3:�����ա��쥻�ץȡ���������
      *             4:���ݡ��쥻�ץȡ�������
      *             5:ϫ�ҡ��쥻�ץȡ�������
      *             6:�����ա��쥻�ץȡ�������
      *             7:�����������ʼ��ݡ�
      *             8:�����������ʹ��ݡ�
      *             9:��������
      *            10:�������μ���
      *            11:�����������μ���
      *            12:���������ٽ�
      *            13:���޾����󶡽�
      *            14:�������쥻�ץȡ���������
      *            15:�������쥻�ץȡ�������
      *            16:�����������ʸ�����
      *            17:�����������ʹ���Ϣ���
      *            18:������Ģ
      *            19:�쥻�ץ��Ż�
      *            20:�ɾ��ܵ�
      *            21:���ǥ����������ս�
      *            22:ͽ��ɼ
      *            23:������������ϫ�ҡ�
      *            24:��ʧ������
      *            25:�����������ٽ�
               05  SYS-1900-PRTKBN-TBL  OCCURS  30.
      *
                   07  SYS-1900-PRTKBN  PIC  X(02).
      *ͽ��
           03  SYS-1900-YOBI            PIC  X(440).
      *ü���ɣ�
           03  SYS-1900-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1900-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1900-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1900-UPYMD           PIC  X(08).
      *��������
           03  SYS-1900-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1900-UPD-KEY.
               05  SYS-1900-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1900-UPD-KANRICD     PIC X(04).
               05  SYS-1900-UPD-KBNCD       PIC X(08).
               05  SYS-1900-UPD-STYUKYMD    PIC X(08).
               05  SYS-1900-UPD-EDYUKYMD    PIC X(08).
