      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       01  SC07.
      *��������
           03  SC07-PRTYM             PIC  X(16).
      *����ɽ��ʬ
           03  SC07-TENHYOKBN1        PIC  X(02).
           03  SC07-TENHYOKBN2        PIC  X(02).
           03  SC07-TENHYOKBN3        PIC  X(02).
           03  SC07-TENHYOKBN4        PIC  X(02).
      *���ŵ��إ�����
           03  SC07-HOSPCD            PIC  X(14).
      *���ŵ��ؽ����
           03  SC07-ADRS              PIC  X(100).
      *���ŵ���̾��
           03  SC07-HOSPNAME          PIC  X(100).
      *�����ֹ�
           03  SC07-TEL               PIC  X(15).
      *������̾
           03  SC07-KAISETUNAME       PIC  X(80).
      *��������
           03  SC07-SEIYMD            PIC  X(22).
      *
           03  SC07-MEISAI-TBL OCCURS 8.
      *��ô���ֹ�
               05  SC07-FTNJANUM      PIC  X(08).
      *���ܤε��ա����
               05  SC07-KENSU         PIC  X(03).
      *���ܤε��ա�����
               05  SC07-TENSU         PIC  X(09).
      *���ް�����ô�⡧���
               05  SC07-YKZKENSU      PIC  X(03).
      *���ް�����ô�⡧���
               05  SC07-YKZFTN        PIC  X(09).
      *������ô�⡧���
               05  SC07-ITBKENSU      PIC  X(03).
      *������ô�⡧���
               05  SC07-ITBFTN        PIC  X(09).
      *�������ܡ����
               05  SC07-SKJKENSU      PIC  X(03).
      *�������ܡ�ɸ����ô��
               05  SC07-SKJFTN        PIC  X(09).
      *���
           03  SC07-GOKEI-TBL.
      *��ס����ܤε��ա����
               05  SC07-GKENSU        PIC  X(03).
      *��ס����ܤε��ա�����
               05  SC07-GTENSU        PIC  X(10).
      *��ס����ް�����ô�⡧���
               05  SC07-GYKZKENSU     PIC  X(03).
      *��ס����ް�����ô�⡧����
               05  SC07-GYKZFTN       PIC  X(10).
      *��סݰ�����ô�⡧���
               05  SC07-GITBKENSU     PIC  X(03).
      *��סݰ�����ô�⡧����
               05  SC07-GITBFTN       PIC  X(10).
      *��סݿ������ܡ����
               05  SC07-GSKJKENSU     PIC  X(03).
      *��סݿ������ܡ�ɸ����ô��
               05  SC07-GSKJFTN       PIC  X(10).
      *�Ʒ���ݷ��
               05  SC07-SAIKEI-TBL  OCCURS  12.
                   07  SC07-SAIKEI    PIC  X(03).
