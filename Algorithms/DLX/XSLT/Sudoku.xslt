<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="text"/>
    <xsl:param name="board"/>

    <!-- Helper to check validity -->
    <xsl:template name="is-valid">
        <xsl:param name="b"/>
        <xsl:param name="idx"/>
        <xsl:param name="val"/>
        
        <xsl:variable name="r" select="floor(($idx - 1) div 9)"/>
        <xsl:variable name="c" select="($idx - 1) mod 9"/>
        
        <!-- Row check -->
        <xsl:variable name="row-str" select="substring($b, $r * 9 + 1, 9)"/>
        
        <xsl:choose>
            <xsl:when test="contains($row-str, $val)">0</xsl:when>
            <xsl:otherwise>
                <!-- Col check -->
                <xsl:variable name="col-str">
                    <xsl:call-template name="get-col">
                        <xsl:with-param name="b" select="$b"/>
                        <xsl:with-param name="c" select="$c"/>
                    </xsl:call-template>
                </xsl:variable>
                <xsl:choose>
                    <xsl:when test="contains($col-str, $val)">0</xsl:when>
                    <xsl:otherwise>
                        <!-- Box check -->
                        <xsl:variable name="br" select="floor($r div 3) * 3"/>
                        <xsl:variable name="bc" select="floor($c div 3) * 3"/>
                        <xsl:variable name="box-str">
                            <xsl:call-template name="get-box">
                                <xsl:with-param name="b" select="$b"/>
                                <xsl:with-param name="br" select="$br"/>
                                <xsl:with-param name="bc" select="$bc"/>
                            </xsl:call-template>
                        </xsl:variable>
                        <xsl:choose>
                            <xsl:when test="contains($box-str, $val)">0</xsl:when>
                            <xsl:otherwise>1</xsl:otherwise>
                        </xsl:choose>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="get-col">
        <xsl:param name="b"/>
        <xsl:param name="c"/>
        <xsl:param name="i" select="0"/>
        <xsl:if test="$i &lt; 9">
            <xsl:value-of select="substring($b, $i * 9 + $c + 1, 1)"/>
            <xsl:call-template name="get-col">
                <xsl:with-param name="b" select="$b"/>
                <xsl:with-param name="c" select="$c"/>
                <xsl:with-param name="i" select="$i + 1"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>

    <xsl:template name="get-box">
        <xsl:param name="b"/>
        <xsl:param name="br"/>
        <xsl:param name="bc"/>
        <xsl:param name="i" select="0"/>
        <xsl:if test="$i &lt; 3">
            <xsl:value-of select="substring($b, ($br + $i) * 9 + $bc + 1, 3)"/>
            <xsl:call-template name="get-box">
                <xsl:with-param name="b" select="$b"/>
                <xsl:with-param name="br" select="$br"/>
                <xsl:with-param name="bc" select="$bc"/>
                <xsl:with-param name="i" select="$i + 1"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>

    <!-- Recursive Solver -->
    <xsl:template name="solve">
        <xsl:param name="b"/>
        <xsl:param name="idx" select="1"/>
        <xsl:param name="iters" select="0"/>

        <xsl:choose>
            <xsl:when test="$idx &gt; 81">
                <xsl:value-of select="concat($b, '|', $iters)"/>
            </xsl:when>
            <xsl:when test="substring($b, $idx, 1) != '0'">
                <xsl:call-template name="solve">
                    <xsl:with-param name="b" select="$b"/>
                    <xsl:with-param name="idx" select="$idx + 1"/>
                    <xsl:with-param name="iters" select="$iters"/>
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
                <xsl:call-template name="try-values">
                    <xsl:with-param name="b" select="$b"/>
                    <xsl:with-param name="idx" select="$idx"/>
                    <xsl:with-param name="val" select="1"/>
                    <xsl:with-param name="iters" select="$iters"/>
                </xsl:call-template>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="try-values">
        <xsl:param name="b"/>
        <xsl:param name="idx"/>
        <xsl:param name="val"/>
        <xsl:param name="iters"/>

        <xsl:choose>
            <xsl:when test="$val &lt;= 9">
                <xsl:variable name="new-iters" select="$iters + 1"/>
                <xsl:variable name="valid">
                    <xsl:call-template name="is-valid">
                        <xsl:with-param name="b" select="$b"/>
                        <xsl:with-param name="idx" select="$idx"/>
                        <xsl:with-param name="val" select="$val"/>
                    </xsl:call-template>
                </xsl:variable>

                <xsl:choose>
                    <xsl:when test="$valid = '1'">
                        <xsl:variable name="new-board" select="concat(substring($b, 1, $idx - 1), $val, substring($b, $idx + 1))"/>
                        <xsl:variable name="res">
                            <xsl:call-template name="solve">
                                <xsl:with-param name="b" select="$new-board"/>
                                <xsl:with-param name="idx" select="$idx + 1"/>
                                <xsl:with-param name="iters" select="$new-iters"/>
                            </xsl:call-template>
                        </xsl:variable>
                        
                        <xsl:choose>
                            <xsl:when test="contains($res, '|')">
                                <xsl:value-of select="$res"/>
                            </xsl:when>
                            <xsl:otherwise>
                                <xsl:call-template name="try-values">
                                    <xsl:with-param name="b" select="$b"/>
                                    <xsl:with-param name="idx" select="$idx"/>
                                    <xsl:with-param name="val" select="$val + 1"/>
                                    <xsl:with-param name="iters" select="$res"/>
                                </xsl:call-template>
                            </xsl:otherwise>
                        </xsl:choose>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:call-template name="try-values">
                            <xsl:with-param name="b" select="$b"/>
                            <xsl:with-param name="idx" select="$idx"/>
                            <xsl:with-param name="val" select="$val + 1"/>
                            <xsl:with-param name="iters" select="$new-iters"/>
                        </xsl:call-template>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="$iters"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template match="/">
        <xsl:variable name="result">
            <xsl:call-template name="solve">
                <xsl:with-param name="b" select="$board"/>
            </xsl:call-template>
        </xsl:variable>
        
        <xsl:choose>
            <xsl:when test="contains($result, '|')">
                <xsl:variable name="final-board" select="substring-before($result, '|')"/>
                <xsl:variable name="final-iters" select="substring-after($result, '|')"/>
                <xsl:text>Puzzle:&#10;</xsl:text>
                <xsl:call-template name="print-formatted">
                    <xsl:with-param name="b" select="$final-board"/>
                </xsl:call-template>
                <xsl:text>&#10;Solved in Iterations=</xsl:text>
                <xsl:value-of select="$final-iters"/>
                <xsl:text>&#10;</xsl:text>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>No solution found.&#10;</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="print-formatted">
        <xsl:param name="b"/>
        <xsl:param name="i" select="0"/>
        <xsl:if test="$i &lt; 9">
            <xsl:call-template name="print-row">
                <xsl:with-param name="r" select="substring($b, $i * 9 + 1, 9)"/>
            </xsl:call-template>
            <xsl:text>&#10;</xsl:text>
            <xsl:call-template name="print-formatted">
                <xsl:with-param name="b" select="$b"/>
                <xsl:with-param name="i" select="$i + 1"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>

    <xsl:template name="print-row">
        <xsl:param name="r"/>
        <xsl:param name="i" select="1"/>
        <xsl:if test="$i &lt;= 9">
            <xsl:value-of select="substring($r, $i, 1)"/>
            <xsl:text> </xsl:text>
            <xsl:call-template name="print-row">
                <xsl:with-param name="r" select="$r"/>
                <xsl:with-param name="i" select="$i + 1"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>

</xsl:stylesheet>
