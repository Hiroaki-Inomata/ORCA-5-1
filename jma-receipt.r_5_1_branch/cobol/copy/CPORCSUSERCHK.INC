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
      *----------------------------------------------------------*
      *    �桼���ץ�����൯ư�ѥ�᥿      COPY (CPORCSUSERCHK.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.00    NACL-¿��Ǽ  16/06/XX  ��ɼ�ֹ��ɲ��б�
      ************************************************************
      *
       01  ORCSUSERCHKAREA.
      *
      *�����У�
           03  ORCSUSERCHK-GMNPG            PIC X(08).
      *������ʬ(���������å��������¹ԡ�
           03  ORCSUSERCHK-SYORI            PIC X(01).
      *�¹Ԥλ��Τ�
           03  ORCSUSERCHK-IN-AREA.
      *��������(�����������������������������
      *�����ౡ��Ͽ��̳�϶�̳�ν�����ʬ��01:�������ˤȤʤ롣
               05  ORCSUSERCHK-SYORI-FLG        PIC 9(02).
      *�����ƥ���������
               05  ORCSUSERCHK-SYSYMD           PIC X(08).
      *������
               05  ORCSUSERCHK-SRYYMD           PIC X(08).
      *���ԣɣ�
               05  ORCSUSERCHK-PTID             PIC 9(10).
      *�����ֹ�
               05  ORCSUSERCHK-PTNUM            PIC X(20).
      *���Ų�
               05  ORCSUSERCHK-SRYKA            PIC X(02).
      *�ɥ�����
               05  ORCSUSERCHK-DRCD             PIC X(05).
      *�ݸ��ȹ礻
               05  ORCSUSERCHK-HKNCOMBI         PIC X(04).
      *��ɼ�ֹ��K03��
               05  ORCSUSERCHK-DENPNUM          PIC 9(07).
      *
      *����
      *    ��������ɽ������
           03  ORCSUSERCHK-GMNSYORI         PIC X(01).
      *    ���̽��ɽ��
           03  ORCSUSERCHK-GYOUMU-FLG       PIC X(01).
      *    1:�¹Ԥ���
           03  ORCSUSERCHK-GYOUMU-ARI       PIC X(01).
      *
      *