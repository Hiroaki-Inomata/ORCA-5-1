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
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������   ����        ����
      *  01.00.01    ���     15/06/08    �����η���ѹ�
      ************************************************************
       01  SC03.
      *��������
           03  SC03-PRTYM             PIC  X(16).
      *����ɽ��ʬ
           03  SC03-TENHYOKBN1        PIC  X(02).
           03  SC03-TENHYOKBN2        PIC  X(02).
           03  SC03-TENHYOKBN3        PIC  X(02).
           03  SC03-TENHYOKBN4        PIC  X(02).
      *���ŵ��إ�����
           03  SC03-HOSPCD            PIC  X(14).
      *���ŵ��ؽ����
           03  SC03-ADRS              PIC  X(100).
      *���ŵ���̾��
           03  SC03-HOSPNAME          PIC  X(100).
      *�����ֹ�
           03  SC03-TEL               PIC  X(15).
      *������̾
           03  SC03-KAISETUNAME       PIC  X(80).
      *��������
           03  SC03-SEIYMD            PIC  X(22).
      *����ơ��֥�
           03  SC03-SEIKYU-TBL   OCCURS  8.
      *��ô���ֹ�
               05  SC03-FTNJANUM      PIC  X(08).
      *���
      *=== �����η���ѹ� by Monma(2015/06/08) start
      *         05  SC03-KENSU         PIC  X(03).
               05  SC03-KENSU         PIC  X(05).
      *=== �����η���ѹ� by Monma(2015/06/08) end
      *����
               05  SC03-TENSU         PIC  X(09).
      *���ް�����ô�⡧���
      *=== �����η���ѹ� by Monma(2015/06/08) start
      *         05  SC03-YKZKENSU      PIC  X(03).
               05  SC03-YKZKENSU      PIC  X(05).
      *=== �����η���ѹ� by Monma(2015/06/08) end
      *���ް�����ô�⡧���
               05  SC03-YKZFTN        PIC  X(09).
      *�������ܡ����
      *=== �����η���ѹ� by Monma(2015/06/08) start
      *         05  SC03-SKJKENSU      PIC  X(03).
               05  SC03-SKJKENSU      PIC  X(05).
      *=== �����η���ѹ� by Monma(2015/06/08) end
      *�������ܡ����
               05  SC03-SKJFTN        PIC  X(09).
      *���
           03  SC03-GOKEI-TBL.
      *��סݷ��
      *=== �����η���ѹ� by Monma(2015/06/08) start
      *         05  SC03-GKENSU        PIC  X(03).
               05  SC03-GKENSU        PIC  X(05).
      *=== �����η���ѹ� by Monma(2015/06/08) end
      *��ס�����
               05  SC03-GTENSU        PIC  X(10).
      *��ס����ް�����ô�⡧���
      *=== �����η���ѹ� by Monma(2015/06/08) start
      *         05  SC03-GYKZKENSU     PIC  X(03).
               05  SC03-GYKZKENSU     PIC  X(05).
      *=== �����η���ѹ� by Monma(2015/06/08) end
      *��ס����ް�����ô�⡧���
               05  SC03-GYKZFTN       PIC  X(10).
      *��סݿ������ܡ����
      *=== �����η���ѹ� by Monma(2015/06/08) start
      *         05  SC03-GSKJKENSU     PIC  X(03).
               05  SC03-GSKJKENSU     PIC  X(05).
      *=== �����η���ѹ� by Monma(2015/06/08) end
      *��סݿ������ܡ�ɸ����ô��
               05  SC03-GSKJFTN       PIC  X(10).
      *�Ʒ���
           03  SC03-SAIKEI-TBL   OCCURS  5.
      *�Ʒ���ݷ��
      *=== �����η���ѹ� by Monma(2015/06/08) start
      *         05  SC03-SAIKEI        PIC  X(03).
               05  SC03-SAIKEI        PIC  X(05).
      *=== �����η���ѹ� by Monma(2015/06/08) end
