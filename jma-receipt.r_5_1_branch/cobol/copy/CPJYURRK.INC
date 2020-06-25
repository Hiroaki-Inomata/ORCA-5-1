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
      *    ��������    ������ COPY  (CPJYURRK.INC)    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-¿��Ǽ   02/04/10  ϫ���ɲ�
      *  01.00.02    NACL-¿��Ǽ  02/10/21  ������ʬ�ɲ�
      *  01.00.03    NACL-��Ʀ��  04/07/01  �ǽ���ʬ���
      *                                     �ޤȤ�������ɼ�ֹ��ɲ�
      *                                     �ޤȤ�����Ϣ���ɲ�
      ************************************************************
      *01  JYURRK-REC.
      *��������
           03  JYURRK-KEY.
      *  ���ŵ��أɣ�
               05  JYURRK-HOSPNUM           PIC 9(2).
      *  ���ԣɣ�
               05  JYURRK-PTID             PIC  9(10).
      *  ������ʬ
               05  JYURRK-NYUGAIKBN        PIC  X(01).
      *  ���Ų�
               05  JYURRK-SRYKA            PIC  X(02).
      *  ������
               05  JYURRK-SRYYMD           PIC  X(08).
      *  Ϣ�֡ʣ������Ʊ��Ϣ�֡�
               05  JYURRK-RENNUM           PIC  9(01).
      *  Ʊ������Ϣ�֡ʣ�����³�ʤ����������Ϣ�֡���³�����
               05  JYURRK-DOUJI-RENNUM     PIC  9(01).
      *  ���Ϣ�֡ʲ�׾���������Ϣ�֡�
               05  JYURRK-KAIKEI-RENNUM    PIC  9(03).
      *  ���֡ʣ����ţ����ްʾ�λ��ˣ������Ϣ�֡�
               05  JYURRK-EDANUM           PIC  9(01).
      *�ɥ�����
           03  JYURRK-DRCD                 PIC X(05).
      *�ݸ��ȹ��ֹ�
           03  JYURRK-HKNCOMBINUM          PIC X(04).
      *�ݸ���ʬ�ʣ������̡�����ϫ�ҡ����������ա��������ʡ�̤���ѡ�
      *          ���������աʷ��ݽ��ˡ�������̳�ҳ��ʷ��ݽ��ˡ�
      *          ����������
           03  JYURRK-HKNKBN               PIC X(01).
      *��ɼ�ֹ�
           03  JYURRK-DENPNUM              PIC 9(07).
      *�ޤȤ�������ɼ�ֹ�
           03  JYURRK-GRP-DENPNUM          PIC 9(07).
      *�ޤȤ�����Ϣ��
           03  JYURRK-GRP-RENNUM           PIC 9(02).
      *��Ǽ������ʬ
           03  JYURRK-SYU-UPDKBN           PIC  X(01).
      *���Ŷ�ʬ
           03  JYURRK-SRYKBN-G.
      *  ���Ŷ�ʬ��
             05  JYURRK-SRYKBN1            PIC  X(02).
      *  ���Ŷ�ʬ��
             05  JYURRK-SRYKBN2            PIC  X(02).
      *  ���Ŷ�ʬ��
             05  JYURRK-SRYKBN3            PIC  X(02).
      *  ���Ŷ�ʬ��
             05  JYURRK-SRYKBN4            PIC  X(02).
      *  ���Ŷ�ʬ��
             05  JYURRK-SRYKBN5            PIC  X(02).
      *  ���Ŷ�ʬ��
             05  JYURRK-SRYKBN6            PIC  X(02).
      *  ���Ŷ�ʬ��
             05  JYURRK-SRYKBN7            PIC  X(02).
      *  ���Ŷ�ʬ��
             05  JYURRK-SRYKBN8            PIC  X(02).
      *  ���Ŷ�ʬ��
             05  JYURRK-SRYKBN9            PIC  X(02).
      *  ���Ŷ�ʬ����
             05  JYURRK-SRYKBN10           PIC  X(02).
      *  ���Ŷ�ʬ����
             05  JYURRK-SRYKBN11           PIC  X(02).
      *
           03  JYURRK-SRYKBN-R         REDEFINES   JYURRK-SRYKBN-G.
      * ���Ŷ�ʬ
             05  JYURRK-SRYKBN             PIC  X(02)
                                           OCCURS   11.
      *���ֹ�
           03  JYURRK-ZAINUM-G.
      *  ���ֹ棱
             05  JYURRK-ZAINUM1            PIC  9(08).
      *  ���ֹ棲
             05  JYURRK-ZAINUM2            PIC  9(08).
      *  ���ֹ棳
             05  JYURRK-ZAINUM3            PIC  9(08).
      *  ���ֹ棴
             05  JYURRK-ZAINUM4            PIC  9(08).
      *  ���ֹ棵
             05  JYURRK-ZAINUM5            PIC  9(08).
      *  ���ֹ棶
             05  JYURRK-ZAINUM6            PIC  9(08).
      *  ���ֹ棷
             05  JYURRK-ZAINUM7            PIC  9(08).
      *  ���ֹ棸
             05  JYURRK-ZAINUM8            PIC  9(08).
      *  ���ֹ棹
             05  JYURRK-ZAINUM9            PIC  9(08).
      *  ���ֹ棱��
             05  JYURRK-ZAINUM10           PIC  9(08).
      *  ���ֹ棱��
             05  JYURRK-ZAINUM11           PIC  9(08).
      *  ���ֹ棱��
             05  JYURRK-ZAINUM12           PIC  9(08).
      *  ���ֹ棱��
             05  JYURRK-ZAINUM13           PIC  9(08).
      *  ���ֹ棱��
             05  JYURRK-ZAINUM14           PIC  9(08).
      *  ���ֹ棱��
             05  JYURRK-ZAINUM15           PIC  9(08).
      *
           03  JYURRK-ZAINUM-R         REDEFINES   JYURRK-ZAINUM-G.
      *  ���ֹ�
             05  JYURRK-ZAINUM             PIC  9(08)
                                           OCCURS 15.
      *ü���ɣ�
           03  JYURRK-TERMID               PIC  X(16).
      *���ڡ��졼���ɣ�
           03  JYURRK-OPID                 PIC  X(16).
      *����ǯ����
           03  JYURRK-CREYMD               PIC  X(08).
      *����ǯ����
           03  JYURRK-UPYMD                PIC  X(08).
      *��������
           03  JYURRK-UPHMS                PIC  X(06).
      *�����祭�������ѹ���
           03  JYURRK-UPKEY.
               05  JYURRK-UPHOSPNUM           PIC 9(2).
               05  JYURRK-UPPTID             PIC  9(10).
               05  JYURRK-UNYUGAIKBN         PIC  X(01).
               05  JYURRK-UPSRYKA            PIC  X(02).
               05  JYURRK-UPSRYYMD           PIC  X(08).
               05  JYURRK-UPRENNUM           PIC  9(01).
               05  JYURRK-UPDOUJI-RENNUM     PIC  9(01).
               05  JYURRK-UPKAIKEI-RENNUM    PIC  9(03).
               05  JYURRK-UPEDANUM           PIC  9(01).