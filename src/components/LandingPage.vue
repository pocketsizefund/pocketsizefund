<template>
    <body>
        <div class="container">
            <div class="top">
                <div class="logo">
                    psf
                </div>
                <div class="link-twitter">
                    <a 
                        href="https://twitter.com/pocketsizefund" 
                        target="_blank"
                    >
                        Twitter
                    </a>
                </div>
            </div>
            <div class="middle">
                <div class="introduction">
                    Recreational quantitative trading 🍊
                </div>
            </div>
        </div>
    </body>
</template>

<script>
export default {
    name: "LandingPage",
};
</script>

<style>
body {
    font-family: 'Lexend', sans-serif;
    background-image: url('../assets/background-oranges.jpg');
    background-repeat: no-repeat;
    background-size: cover;
    background-attachment: fixed;
    height: 100%;
    width: 100%;
}

.container {
    height: 100%;
    margin: auto;
    width: 34rem;
    padding: 2rem 1rem 0rem;

}

.top > div {
    display: inline-block;
}

.logo {
    font-size: 3rem;
    font-weight: 900;
    color: #ffffff;
}

.link-twitter {
    float: right;
    font-size: 1rem;
    font-weight: 500;
}

a:link,
a:visited,
a:hover,
a:active {
    text-decoration: none;
    color: #ffffff;
}

.middle {
    padding-top: 6rem;
}

.introduction {
    font-size: 4rem;
    font-weight: 900;
    color: #ffffff;
    align-content: center;
}

@media screen and (max-width: 580px) {
    .container {
        width: 100%;
    }

    .middle {
        padding-top: 2rem;
    }

    .introduction {
        font-size: 2.5rem;
        font-weight: 900;
        color: #ffffff;
        align-content: center;
    }
}
</style>