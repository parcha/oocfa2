         J   I        ��������K�|J�G�{�o��h�i�            u#!/bin/sh
LD_LIBRARY_PATH=lib java -Xmx512M -jar sbt-launch-0.7.5.jar $@
     J     ,   +          ������E�+��]36�0�r�O            u#!/bin/sh
java -Xmx512M -jar ${SBT_JAR} $@
     v     ]   t         ������mV]�gn	g�gk Y{O�            x�c` . VV�O���OJ,���LS�VPR�v
��r�UR��UPRR��V(�H�S�RP�J�*'���$��%g����e%)q�e*pq ��     �     -   t      f   ������@�W1���i��Nj               -   N   !  SBT_JAR="sbt-launch-0.7.7.jar"
          -   t      l   ����:]G�h|��_'�\O��&               -   N   !  SBT_JAR="sbt-launch-0.7.5.jar"
    -     `   �      �   ������-��d�?��Ǐ�d�:�0��               S   t   Tjava -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256m -Xmx512M -jar ${SBT_JAR} $@
    �     \   [      �   �������;�SVS���ˎ^�=            ujava -Xmx712M -Xss2M -XX:+CMSClassUnloadingEnabled -jar `dirname $0`/sbt-launcher.jar "$@"
